library(magrittr)

path_data <- project_root$find_file("data/ihdp_obs.csv")
df <- read.table(path_data, header = TRUE)

df_juice <- df %>%
    recipes::recipe() %>%
    recipes::update_role(dplyr::starts_with("X"), new_role = "predictor") %>%
    recipes::update_role(treatment, new_role = "outcome") %>%
    recipes::update_role(y_factual, new_role = "id variable") %>%
    # recipes::step_interact(terms = ~ X6:X4) %>%
    recipes::step_normalize(recipes::all_numeric_predictors() & !where(is.integer)) %>%
    recipes::prep() %>%
    recipes::juice()

#------------------------------------
# propensity score
#------------------------------------

# match data with treatment:control = 1:2 (as specified by ratio parameter)
match.out <- MatchIt::matchit(formula = treatment ~ . - y_factual, data = df_juice, distance = "glm", method = "optimal", ratio = 2)

if (integrate()) {
    # check balancing
    library(cobalt)
    # show balance table
    cobalt::bal.tab(match.out, thresholds = c(m = .1, v = 2), imbalanced.only = TRUE)

    # show love plot
    cobalt::love.plot(match.out, binary = "std", thresholds = c(m = .1))

    # density plot by cobalt, which cannot display multi densities at the same time
    cobalt::bal.plot(match.out, var.name = "X21", which = "both")
    # this shows a few density plots at the same time
    match.out %>% plot(type = "density", interactive = FALSE, which.xs = c("X1", "X2"))
}

#------------------------------------
# outcome model
#------------------------------------

# estimate ATT
outcome_model_trained <- lm(y_factual ~ ., data = match.out %>% MatchIt::match.data(), weights = weights)
ATT_est <- outcome_model_trained$coefficients["treatment"]
ATT_est %>% print()

# cluster-robust standard deviation
cluster_robust_stat <- lmtest::coeftest(outcome_model_trained, vcov. = sandwich::vcovCL, cluster = ~subclass)

cluster_robust_CI <- lmtest::coefci(outcome_model_trained, level = 0.95, vcov. = sandwich::vcovCL, cluster = ~subclass)
cluster_robust_CI["treatment", ]
