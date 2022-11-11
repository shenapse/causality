library(magrittr)

path_data <- project_root$find_file("data/ihdp_obs.csv")
df <- read.table(path_data, header = TRUE)
#-------------------------------
# treatment model
#-------------------------------
estimand <- "ATE"

df_juice <- recipes::recipe(df, treatment ~ .) %>%
    # add interaction term for reduce in-balance (see balance check below)
    # recipes::step_interact(terms = ~ X6:X4) %>%
    recipes::step_interact(terms = ~ -y_factual) %>%
    recipes::step_normalize(-where(is.integer), -y_factual) %>%
    recipes::prep() %>%
    recipes::juice()

W.out <- WeightIt::weightit(treatment ~ . - y_factual, data = df_juice, estimand = estimand, method = "ps")
# W.out %>% summary()
# balancing diagnosis
library(cobalt)
cobalt::set.cobalt.options(imbalanced.only = TRUE)
cobalt::bal.tab(W.out, stats = c("m", "v"), thresholds = c(m = .05), estimand = estimand)
cobalt::love.plot(W.out, thresholds = c(m = .05))
cobalt::bal.plot(W.out, var.name = "X6", which = "both")

#-------------------------------
# outcome model
#-------------------------------

xgb_recipe <- df_juice %>%
    recipes::recipe(y_factual ~ .)

df_outcome <- xgb_recipe %>%
    recipes::prep() %>%
    recipes::juice()

# fix model spec
xgb_spec <- parsnip::boost_tree(
    trees = 500,
    tree_depth = 10,
    min_n = 10,
    loss_reduction = 0,
    sample_size = 0.3,
    mtry = 10,
    learn_rate = .01
) %>%
    parsnip::set_engine("xgboost") %>%
    parsnip::set_mode("regression")


xgb_wf <- workflows::workflow() %>%
    workflows::add_recipe(xgb_recipe) %>%
    workflows::add_model(xgb_spec)


get_trained_xgb_model <- function(data) {
    recipe_part <- data %>% recipes::recipe(y_factual ~ .)
    df_juice <- recipe_part %>%
        recipes::prep() %>%
        recipes::juice()
    xgb_wf <- workflows::workflow() %>%
        workflows::add_recipe(recipe_part) %>%
        workflows::add_model(xgb_spec)
    xgb_wf %>%
        generics::fit(df_juice) %>%
        return()
}

est_DRE <- function(data) {
    # train outcome models on treated and control, and then get the 2 predictions on data
    w.out <- WeightIt::weightit(treatment ~ . - y_factual, data = data, estimand = estimand, method = "ps")
    data_ps <- data %>% dplyr::bind_cols(w.out$ps %>% as.data.frame() %>% magrittr::set_colnames(c("ps")))
    df1 <- data_ps %>% dplyr::filter(treatment == 1)
    df0 <- data_ps %>% dplyr::filter(treatment == 0)
    model1 <- get_trained_xgb_model(df1)
    model0 <- get_trained_xgb_model(df0)
    # calc DRE
    y_pred1 <- predict(model1, data_ps) %>% dplyr::pull(.pred)
    y_pred0 <- predict(model0, data_ps) %>% dplyr::pull(.pred)
    ps <- w.out$ps
    y <- data %>% dplyr::pull(y_factual)
    treatment <- data %>% dplyr::pull(treatment)
    DRE <- mean(treatment * y / ps - (treatment - ps) * y_pred1 / ps) - mean((1 - treatment) * y / (1 - ps) - (treatment - ps) * y_pred1 / (1 - ps))
    return(DRE)
}

# estimate ATE
set.seed(13)
tictoc::tic()
ATE_DRE <- est_DRE(df_juice)
tictoc::toc()
ATE_DRE %>% print()

# bootstrap by rsample
total_rep <- 1000
resample <- rsample::bootstraps(data = df_juice, times = total_rep)
# parallel computation
library(doParallel)
cores <- parallel::detectCores() - 1
cl <- parallel::makeCluster(cores)
doParallel::registerDoParallel(cl)

# time stamp
Sys.time() %>% print()
tictoc::tic() # time measurement starts
# calc
res <- foreach::foreach(i = 1:total_rep, .combine = "c", .packages = c("magrittr", "rsample", "recipes", "parsnip", "workflows", "generics", "xgboost", "dplyr", "WeightIt")) %dopar% {
    resample$splits[i] %>%
        as.data.frame() %>%
        est_DRE()
}
tictoc::toc() # time measurement ends
parallel::stopCluster(cl)
res %>% mean()
# percentile 95% confidence interval
res %>% quantile(probs = c(.025, .975))
