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

xgb_recipe <- df %>%
    dplyr::bind_cols(W.out$ps %>% as.data.frame() %>% magrittr::set_colnames(c("ps"))) %>%
    recipes::recipe(y_factual ~ .)

df_outcome <- xgb_recipe %>%
    recipes::prep() %>%
    recipes::juice()

split <- rsample::initial_split(df_outcome, prop = .8)

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

# xgb_trained <- xgb_wf %>% tune::last_fit(split, metrics = yardstick::metric_set(yardstick::mae, yardstick::rmse, yardstick::rsq))

# xgb_trained %>% tune::collect_metrics()

# re-write this func so that it returns 2 outcome models and 1 df
get_trained_xgb_model <- function(data) {
    recipe_part <- data %>% recipes::recipe(y_factual ~ .)
    df_juice <- recipe_part %>%
        recipes::prep() %>%
        recipes::juice()
    xgb_wf <- workflows::workflow() %>%
        workflows::add_recipe(recipe_part) %>%
        workflows::add_model(xgb_spec)
    xgb_fit <- xgb_wf %>% generics::fit(df_juice)
    return(xgb_fit)
}
# predict(ret$model, ret$df)
# test <- df_outcome %>% dplyr::bind_cols(predict(xgb_fit, df_outcome))

est.fn <- function(data, index) {
    # train outcome models on treated and control, and then get the 2 predictions on data
    w.out <- WeightIt::weightit(treatment ~ . - y_factual, data = data, estimand = estimand, method = "ps")
    data_ps <- data %>% dplyr::bind_cols(w.out$ps %>% as.data.frame() %>% magrittr::set_colnames(c("ps")))
    df1 <- data_ps[index, ] %>% dplyr::filter(treatment == 1)
    df0 <- data_ps[index, ] %>% dplyr::filter(treatment == 0)
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

# estimate ATE!
index_whole <- df %>% nrow()
start_DRE <- proc.time() # time measurement starts
ATE_DRE <- est.fn(df_juice, 1:index_whole)
proc.time() - start_DRE # time measurement ends
ATE_DRE %>% print()


# estimate DRE of boot-strapping std
library(parallel)
total_rep <- 10
start_boot <- proc.time() # time measurement starts
boot.out <- boot::boot(statistic = est.fn, data = df_juice, R = total_rep, parallel = "multicore", ncpus = parallel::detectCores())
proc.time() - start_boot # time measurement ends
boot::boot.ci(boot.out, type = "basic")
