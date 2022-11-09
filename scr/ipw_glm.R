library(magrittr)

path_data <- project_root$find_file("data/ihdp_obs.csv")
df <- read.table(path_data, header = TRUE)

# follow the same step as Matching
# until we get glm model that estimates propensity score
# define df for estimating propensity score
df_TX <- df %>% dplyr::select(-y_factual)
# define pre-processing
preprocessor <- recipes::recipe(df_TX, treatment ~ .) %>%
    # add interaction term for reduce in-balance (see balance check below)
    recipes::step_interact(terms = ~ X6:X4) %>%
    recipes::step_normalize(-where(is.integer))

# model specification
glm_spec <- parsnip::set_engine("glm", family = binomial(link = "logit"))

# define workflow
wf <- workflows::workflow() %>%
    workflows::add_recipe(preprocessor) %>%
    workflows::add_model(glm_spec)

# juice dataframe to be used in fit below
df_juice <- preprocessor %>%
    recipes::prep() %>%
    recipes::juice()
# train the model
glm_trained <- generics::fit(wf, df_juice)
# summary of the trained model
broom::tidy(glm_trained, "model")

# new column of predicted propensity score
# add weights
df_weight <- df_juice %>%
    dplyr::bind_cols(predict(glm_trained, df_juice) %>% magrittr::set_colnames(c("p.score"))) %>%
    dplyr::mutate(att.weight = treatment + (1 - treatment) * p.score / (1 - p.score)) %>%
    dplyr::mutate(ate.weight = treatment / p.score + (1 - treatment) / (1 - p.score))

#------------------------
#  balancing check
#------------------------
library(cobalt)
# prepare df and vector for convenience
covs <- df_juice %>% dplyr::select(dplyr::starts_with("X"))
treatment <- df_TX %>% dplyr::pull(treatment)

cobalt::bal.tab(covs, treat = treatment, weights = list(ATT = att.weights, ATE = ate.weights), thresholds = c(m = .1, v = 2), imbalanced.only = TRUE)

# ATT weights looks good while ATE one does not
cobalt::love.plot(x = covs, treat = treatment, weights = list(ATT = att.weights, ATE = ate.weights), binary = "std", thresholds = c(m = .1))

#------------------------
# outcome model
#------------------------

# fit linear model to estimate ATE
# extract weight for later use
att.weights <- df_weight %>% dplyr::pull(att.weight)
ate.weights <- df_weight %>% dplyr::pull(ate.weight)

# define outcome model for estimating ATT and ATE
# define preprocessor and df for outcome model
df_outcome <- df_juice %>%
    dplyr::bind_cols(df %>% dplyr::select(y_factual)) %>%
    recipes::recipe(y_factual ~ .) %>%
    recipes::prep() %>%
    recipes::juice()

# train the model
# we use lm() method as workflows package does not accept weights option
# fit ATT model
att_outcome_model_trained <- lm(y_factual ~ ., data = df_outcome, weights = att.weights)

# ATT = coeff on treatment
# since love.plot shows that ATT weight makes co-variates well-balanced
# the ATT estimated based on that weights seems more reliable than ATE,
# which suffers greater in-balance in love.plot
ATT_est <- att_outcome_model_trained$coefficients["treatment"]
ATT_est %>% print()

# cluster-robust standard deviation
cluster_robust_stat.att <- lmtest::coeftest(att_outcome_model_trained, vcov. = sandwich::vcovCL, cluster = att.weights)
cluster_robust_CI.att <- lmtest::coefci(att_outcome_model_trained, level = 0.95, vcov. = sandwich::vcovCL, cluster = att.weights)
cluster_robust_CI.att %>% print()

# fit ATE model
ate_outcome_model_trained <- lm(y_factual ~ ., data = df_outcome, weights = ate.weights)

# ATE = coeff on treatment
# As love.plot for ATE weight shows,
# estimated ATE is like to have un-ignorable bias
ATE_est <- ate_outcome_model_trained$coefficients["treatment"]
ATE_est %>% print()

# cluster-robust standard deviation
cluster_robust_stat.ate <- lmtest::coeftest(ate_outcome_model_trained, vcov. = sandwich::vcovCL, cluster = ate.weights)
cluster_robust_CI.ate <- lmtest::coefci(ate_outcome_model_trained, level = 0.95, vcov. = sandwich::vcovCL, cluster = ate.weights)
cluster_robust_CI.ate %>% print()

# calc_stats <- function(model, weights) {
#     effect <- model$coefficients["treatment"]
#     cluster_robust_std <- lmtest::coeftest(model, vcov. = sandwich::vcovCL, cluster = weights)
#     cluster_robust_CI <- lmtest::coeftest(model, vcov. = sandwich::vcovCL, cluster = weights)
#     return(list(effect = effect, cluster_robust_std = std, cluster_robust_CI = cluster_robust_CI))
# }
