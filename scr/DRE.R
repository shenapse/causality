library(magrittr)

path_data <- project_root$find_file("data/ihdp_obs.csv")
df <- read.table(path_data, header = TRUE)
#-------------------------------
# treatment model
#-------------------------------
estimand <- "ATE"

ps_recipe <- df %>%
    recipes::recipe() %>%
    recipes::update_role(dplyr::starts_with("X"), new_role = "predictor") %>%
    recipes::update_role(treatment, new_role = "outcome") %>%
    recipes::update_role(y_factual, new_role = "id variable") %>%
    recipes::step_normalize(recipes::all_numeric_predictors() & !where(is.integer))

if (interactive()) {
    df_ps <- ps_recipe %>%
        recipes::prep() %>%
        recipes::juice()

    W.out <- WeightIt::weightit(treatment ~ ., data = df_ps, estimand = estimand, method = "ps")
    # balancing diagnosis
    W.out %>% summary()
    library(cobalt)
    cobalt::set.cobalt.options(imbalanced.only = TRUE)
    cobalt::bal.tab(W.out, stats = c("m", "v"), thresholds = c(m = .05), estimand = estimand)
    cobalt::love.plot(W.out, thresholds = .05)
    cobalt::bal.plot(W.out, var.name = "X6", which = "both")
}
#-------------------------------
# outcome model: GBDT
#-------------------------------

# split data set
set.seed(123)
splits <- rsample::initial_split(df, prop = .8, strata = treatment)
df_train <- rsample::training(splits)
df_test <- rsample::testing(splits)

# bind propensity score to training set
get_df_ps <- function(data) {
    w.out <- WeightIt::weightit(treatment ~ . - y_factual, data = ps_recipe %>% recipes::prep() %>% recipes::bake(new_data = data), estimand = estimand, method = "ps")
    data %>%
        dplyr::bind_cols(w.out$ps %>% as.data.frame() %>% magrittr::set_colnames(c("ps"))) %>%
        return()
}
df_train <- get_df_ps(df_train)
df_test <- get_df_ps(df_test)
df_merge <- dplyr::bind_rows(df_train, df_test)
# split new data frame with propensity score
splits <- rsample::initial_split(df_merge, prop = .8, strata = treatment)
df_train <- rsample::training(splits)
df_test <- rsample::testing(splits)

# Optimize hyper-parameters for GBDT
# assuming that outcome models on treatment and control share the same ones
set.seed(456)
folds <- rsample::vfold_cv(df_train, v = 10, strata = treatment)

# recipe for outcome model
xgb_recipe <- df_train %>%
    recipes::recipe() %>%
    recipes::update_role(-y_factual, new_role = "predictor") %>%
    recipes::update_role(y_factual, new_role = "outcome")

if (interactive()) xgb_recipe

# fix model spec
xgb_spec <- parsnip::boost_tree(
    trees = tune::tune(),
    tree_depth = tune::tune(),
    min_n = tune::tune(),
    loss_reduction = 0,
    sample_size = tune::tune(),
    learn_rate = .01
) %>%
    parsnip::set_engine("xgboost") %>%
    parsnip::set_mode("regression")


xgb_wf <- workflows::workflow() %>%
    workflows::add_recipe(xgb_recipe) %>%
    workflows::add_model(xgb_spec)

xgb_params <- xgb_wf %>%
    tune::extract_parameter_set_dials() %>%
    update(
        sample_size = dials::sample_prop(),
        trees = dials::trees(c(500, 1000)),
        tree_depth = dials::tree_depth(c(10, 15))
    )

# enable parallel computation
library(doParallel)
cores <- parallel::detectCores() - 1
cl <- parallel::makeCluster(cores)
doParallel::registerDoParallel(cl)
set.seed(789)

grid_result <- xgb_wf %>% tune::tune_grid(
    resamples = folds,
    grid = 10,
    param_info = xgb_params,
    control = tune::control_grid(verbose = TRUE)
)

opt_res <- xgb_wf %>% tune::tune_bayes(
    resamples = folds,
    # param_info = xgb_params,
    initial = grid_result,
    param_info = xgb_params,
    iter = 10,
    metrics = yardstick::metric_set(yardstick::rmse),
    control = tune::control_bayes(verbose = TRUE)
)

tune::show_best(opt_res, metric = "rmse")
# this is what we wanted
xgb_best_params <- opt_res %>% tune::select_best("rmse")
xgb_wf_final <- xgb_wf %>% tune::finalize_workflow(xgb_best_params)
# make sure that the model with this parameter performs as good as expected
final_fit <- xgb_wf_final %>% tune::last_fit(splits)
final_fit %>% tune::collect_metrics()

# next construct outcome models on treatment and control
est_DRE <- function(data) {
    # train outcome models on treated and control, and then get the 2 predictions on data
    w.out <- WeightIt::weightit(treatment ~ . - y_factual, data = data, estimand = estimand, method = "ps")
    data_ps <- data %>% dplyr::bind_cols(w.out$ps %>% as.data.frame() %>% magrittr::set_colnames(c("ps")))
    df1 <- data_ps %>% dplyr::filter(treatment == 1)
    df0 <- data_ps %>% dplyr::filter(treatment == 0)
    model1 <- xgb_wf_final %>% generics::fit(df1)
    model0 <- xgb_wf_final %>% generics::fit(df0)
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
ATE_DRE <- est_DRE(df)
tictoc::toc()
ATE_DRE %>% print()

# calculate 95% confidence interval by bootstrap
# bootstrap by rsample
total_rep <- 2000
resample <- rsample::bootstraps(data = df, times = total_rep)

# time stamp
Sys.time() %>% print()
tictoc::tic() # time measurement starts
# parallel computation
boots <- foreach::foreach(i = 1:total_rep, .combine = "c", .packages = c("magrittr", "rsample", "recipes", "parsnip", "workflows", "generics", "xgboost", "dplyr", "WeightIt")) %dopar% {
    resample$splits[i] %>%
        as.data.frame() %>%
        est_DRE()
}
tictoc::toc() # time measurement ends
# show result
boots %>% mean() # 4.657371
boots %>% quantile(probs = c(.025, .975))
# save distribution of bootstrap samples as PNG
library(ggplot2)
title <- stringr::str_c("Distribution of bootstrap samples of doubly-robust estimator. (N=", total_rep, ")")
g <- boots %>%
    as.data.frame() %>%
    magrittr::set_colnames(c("DRE")) %>%
    ggplot(aes(x = DRE)) +
    geom_histogram(aes(y = ..density..), binwidth = .05, fill = "#69b3a2", color = "#e9ecef", alpha = 0.9) +
    geom_density(color = "#69b3a2") +
    ggtitle(title) +
    theme(plot.title = element_text(size = 15))
# save as png
file_name <- stringr::str_c("DRE_bootstrap_", total_rep, ".png")
ggsave(file = project_root$find_file("scr", file_name))

# end parallel computation
parallel::stopCluster(cl)
