library(magrittr)

path_data <- project_root$find_file("data/ihdp_obs.csv")
df <- read.table(path_data, header = TRUE)
df_TX <- df %>%
    dplyr::select(-y_factual)

# define pre-processing
preprocessor <- recipes::recipe(df_TX, treatment ~ .) %>%
    # add interaction term for reduce in-balance (see balance check below)
    recipes::step_interact(terms = ~ X6:X4) %>%
    recipes::step_normalize(-where(is.integer))

# model specification
glm_spec <- parsnip::linear_reg() %>%
    parsnip::set_engine("glm", family = binomial(link = "logit"))

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
df_TXp <- df_juice %>%
    dplyr::bind_cols(predict(glm_trained, df_juice) %>% set_colnames(c("p.score")))

# match data based on propensity score
treatment <- df_juice$treatment
# by Matching package
match.out <- Matching::Match(Y = df %>% dplyr::pull(y_factual), Tr = treatment, X = df_TXp %>% dplyr::pull(p.score), M = 2, estimand = "ATT")

# -----------------------
# check balance of the matching result
# matching package provides Matching::MatchBalance()
# but we use cobalt package instead
# as it is a more widely-applicable tool for checking balance
# with match object from matching package
# -----------------------
# prepare data for balance checking

covs <- df_juice %>% dplyr::select(dplyr::starts_with("X"))
p.score <- df_TXp %>% dplyr::pull(p.score)
# show balance table
cobalt::bal.tab(match.out, treat = treatment, covs = covs, distance = ~p.score, thresholds = c(m = .1, v = 2), imbalanced.only = TRUE)

# int = TRUE and poly = option are useful when exploring interaction term
cobalt::bal.tab(match.out, treat = treatment, covs = covs, distance = ~p.score, thresholds = c(m = .1, v = 2), imbalanced.only = TRUE, int = TRUE, poly = 2)

# density plot
cobalt::bal.plot(match.out, treat = treatment, covs = covs, distance = ~p.score, var.name = "X7", which = "both")

# love plot
love.plot(match.out, treat = treatment, covs = covs, distance = ~p.score, binary = "std", thresholds = c(m = .1))

# estimated ATT
ATT_est <- match.out$est[1, 1]
Matching::summary.Match(match.out)
