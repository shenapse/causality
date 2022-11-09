library(magrittr)

path_data <- project_root$find_file("data/ihdp_obs.csv")
df <- read.table(path_data, header = TRUE)

df_juice <- recipes::recipe(df, treatment ~ .) %>%
    # add interaction term for reduce in-balance (see balance check below)
    # recipes::step_interact(terms = ~ X6:X4) %>%
    recipes::step_interact(terms = ~ -y_factual) %>%
    recipes::step_normalize(-where(is.integer), -y_factual) %>%
    recipes::prep() %>%
    recipes::juice()
# match data with treatment:control = 1:2 (see ratio parameter)
match.out <- MatchIt::matchit(formula = treatment ~ . - y_factual, data = df_juice, distance = "glm", method = "optimal", ratio = 2)

# check balancing
# treatment <- df_juice %>% dplyr::pull(treatment)
# covs <- df_juice %>% dplyr::select(-y_factual)
# show balance table
cobalt::bal.tab(match.out, thresholds = c(m = .1, v = 2), imbalanced.only = TRUE)

# show love plot
love.plot(match.out, binary = "std", thresholds = c(m = .1))

# density plot by cobalt, which cannot display multi densities at the same time
cobalt::bal.plot(match.out, var.name = "X21", which = "both")
# contrary to the plot method
match.out %>% plot(type = "density", interactive = FALSE, which.xs = c("X5", "X12", "X21"))


# estimate ATT
# prepare df for linear model
outcome_model_trained <- lm(y_factual ~ ., data = match.out %>% MatchIt::match.data(), weights = weights)
ATT_est <- outcome_model_trained$coefficients["treatment"]
ATT_est %>% print()

# cluster-robust standard deviation
cluster_robust_stat <- lmtest::coeftest(outcome_model_trained, vcov. = sandwich::vcovCL, cluster = ~subclass)

cluster_robust_CI <- lmtest::coefci(outcome_model_trained, level = 0.95, vcov. = sandwich::vcovCL, cluster = ~subclass)
cluster_robust_CI["treatment", ]
