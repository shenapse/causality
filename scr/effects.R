library(magrittr)

# project_root is defined in .Rprofile
# project_root <- rprojroot::is_git_root
path_data <- project_root$find_file("data/ihdp.csv")
tbl <- read.table(path_data, header = TRUE, stringsAsFactors = FALSE)

# 4.10735380188697
ATT <- read.table(path_data, header = TRUE) %>%
    dplyr::select(treatment, dplyr::starts_with("y")) %>%
    dplyr::filter(treatment == 1) %>%
    dplyr::mutate(diff = y_factual - y_cfactual) %>%
    dplyr::summarise(ATT = diff %>% mean()) %>%
    dplyr::pull(ATT)

# 4.64031740451832
ATE <- read.table(path_data, header = TRUE) %>%
    dplyr::select(treatment, dplyr::starts_with("y")) %>%
    dplyr::mutate(y1 = dplyr::if_else(treatment == 1, y_factual, y_cfactual), y0 = dplyr::if_else(treatment == 0, y_factual, y_cfactual), diff = y1 - y0) %>%
    dplyr::summarise(ATE = diff %>% mean()) %>%
    dplyr::pull(ATE)
