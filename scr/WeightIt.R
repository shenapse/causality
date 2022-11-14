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

# propensity score matching by weightit
estimand <- "ATT"
if (interactive()) {
    W.pscore.out <- WeightIt::weightit(treatment ~ . - y_factual, data = df_juice, estimand = estimand, method = "ps")
    # see how the weights are estimated
    W.pscore.out %>% print()
    # see summary stat of the weighting
    W.pscore.out %>% summary()
}

W.entropy.out <- WeightIt::weightit(treatment ~ . - y_factual, data = df_juice, estimand = estimand, method = "ebal")
# entropy balancing by weigthit
# W.entropy.out %>% summary()

# balancing check by cobalt
if (interactive()) {
    library(cobalt)
    cobalt::set.cobalt.options(imbalanced.only = TRUE, binary = "std")
    # on propensity score version
    cobalt::bal.tab(W.pscore.out, stats = c("m", "v"), thresholds = c(m = .05), imbalanced.only = TRUE)
    # on entropy weighting
    cobalt::bal.tab(W.entropy.out, stats = c("m", "v"), thresholds = c(m = .05))
    cobalt::love.plot(W.entropy.out, , line = TRUE, thresholds = c(m = .05), estimand = estimand)
}

# outcome model
outcome_model_trained <- lm(y_factual ~ ., data = df_juice, weights = W.entropy.out$weights)
coef(outcome_model_trained)["treatment"]

# boot-stropping std
est.fn <- function(data, index) {
    w.out <- WeightIt::weightit(treatment ~ . - y_factual, data = data[index, ], estimand = estimand, method = "ebal")
    model_trained <- lm(y_factual ~ ., data = data[index, ], weights = w.out$weights)
    return(coef(model_trained)["treatment"])
}

set.seed(123)
boot.out <- boot::boot(statistic = est.fn, data = df_juice, R = 2000)
boot::boot.ci(boot.out, type = "bca")
