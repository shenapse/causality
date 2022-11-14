library(magrittr)

path_data <- project_root$find_file("data/ihdp_obs.csv")
df <- read.table(path_data, header = TRUE)

#------------------------------------
# propensity score
#------------------------------------

# df for estimating propensity score
df_TX <- df %>%
    dplyr::select(-y_factual)

# define pre-processing
preprocessor <- df %>%
    recipes::recipe() %>%
    recipes::update_role(dplyr::starts_with("X"), new_role = "predictor") %>%
    recipes::update_role(treatment, new_role = "outcome") %>%
    recipes::update_role(y_factual, new_role = "id variable") %>%
    recipes::step_interact(terms = ~ X6:X4) %>%
    recipes::step_normalize(recipes::all_numeric_predictors() & !where(is.integer))

# add interaction term for reducing in-balance (see balance check below)
if (interactive()) {
    preprocessor %>%
        recipes::prep() %>%
        recipes::juice() %>%
        View()
}

# model specification
glm_spec <- parsnip::linear_reg() %>%
    parsnip::set_engine("glm", family = binomial(link = "logit"))

# define workflow
wf <- workflows::workflow() %>%
    workflows::add_recipe(preprocessor) %>%
    workflows::add_model(glm_spec)

# train the model
glm_trained <- generics::fit(wf, df)

# summary of the trained model
if (interactive()) {
    broom::tidy(glm_trained, "model")
}

# new column of predicted propensity score
df_full <- preprocessor %>%
    recipes::prep() %>%
    recipes::juice() %>%
    dplyr::bind_cols(predict(glm_trained, df) %>% set_colnames(c("p.score")))

# match data based on propensity score
treatment <- df_juice$treatment
# by Matching package
match.out <- Matching::Match(Y = df_juice$y_factual, Tr = treatment, X = df_full %>% dplyr::pull(p.score), M = 2, estimand = "ATT")

# -----------------------------------------------------------
# check balance of the matching result
# matching package provides Matching::MatchBalance()
# but we use cobalt package instead
# as it is a more widely-applicable tool for checking balance
# -----------------------------------------------------------
if (interactive()) {
    # loading cobalt is required for love.plot() to find bal.tab() method
    library(cobalt)

    # prepare variables for balance checking
    covs <- df_full %>% dplyr::select(dplyr::starts_with("X"))
    p.score <- df_full %>% dplyr::pull(p.score)

    # show balance table
    cobalt::bal.tab(match.out, treat = treatment, covs = covs, distance = ~p.score, thresholds = c(m = .1, v = 2), imbalanced.only = TRUE)

    # int = TRUE and poly = option are useful when exploring interaction term
    cobalt::bal.tab(match.out, treat = treatment, covs = covs, distance = ~p.score, thresholds = c(m = .1, v = 2), imbalanced.only = TRUE, int = TRUE, poly = 2)

    # density plot
    cobalt::bal.plot(match.out, treat = treatment, covs = covs, distance = ~p.score, var.name = "X7", which = "both")

    # love plot
    cobalt::love.plot(match.out, treat = treatment, covs = covs, distance = ~p.score, binary = "std", thresholds = c(m = .1))
}

#------------------------------------
# Estimation of ATT
#------------------------------------

ATT_est <- match.out$est[1, 1]
Matching::summary.Match(match.out)
