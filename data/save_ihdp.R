library(magrittr)

# get dataframes and combine them
N <- 10
# targe urls
base_url <- "https://raw.githubusercontent.com/AMLab-Amsterdam/CEVAE/master/datasets/IHDP/csv/ihdp_npci_"
urls <- purrr::map_chr(.x = 1:N, .f = ~ stringr::str_c(base_url, .x, ".csv"))

# get and combine data into single dataframe
df <- NULL
for (i in 1:N) {
    data <- readr::read_csv(RCurl::getURL(urls[i]), col_names = FALSE, show_col_types = FALSE)
    df <- dplyr::bind_cols(df, data)
}

# set column names
cols_named <- c("treatment", "y_factual", "y_cfactual", "mu0", "mu1")
cols_unnamed <- purrr::map_chr(.x = 1:25, .f = ~ stringr::str_c("X", .x))
col_names <- c(cols_named, cols_unnamed)
# bind dataframe with naming cols
colnames(df) <- col_names

# save full dataframe
path <- project_root$find_file("data/ihdp.csv")
# save without row id
write.table(df, path, row.names = FALSE)

# save observed data
# remove mu0, mu1 columns
# mu's are expected value for the corresponding potential outcomes
# remove y_cfactual as it is unobservable
path_obs <- project_root$find_file("data/ihdp_obs.csv")
df %>%
    dplyr::select(-dplyr::starts_with("mu"), -y_cfactual) %>%
    write.table(path_obs, row.names = FALSE)
