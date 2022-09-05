library(magrittr)
find_path_relative_root <- rprojroot::is_git_root$make_fix_file()

# test if the passed path exists.
# `raise_error = ` specifies weather to raise an error if it does not.
test_path <- function(path, raise_error = TRUE) {
    if (!file.exists(path) && !dir.exists(path)) {
        if (!raise_error) {
            return(FALSE)
        }
        msg <- paste("No such file or directory exists:", path)
        stop(msg)
    }
    return(TRUE)
}

test_path2 <- function(path, raise_error = TRUE) {
    # if the path exists
    if (!file.exists(path) && !dir.exists(path)) {
        if (!raise_error) {
            return(FALSE)
        }
        msg <- paste("No such file or directory exists:", path)
        stop(msg)
    }
    # if the path exists within root project
    if (stringr::str_starts(
        string = path,
        pattern = find_path_relative_root(),
        negate = TRUE
    )) {
        if (!raise_error) {
            return(FALSE)
        }
        stop(paste("file is outside project", path))
    }
    return(TRUE)
}

get_dirconfig <- function(name = "dir_config.yml", raise_error = TRUE) {
    path <- find_path_relative_root(name)
    ifelse(
        test_path(path, raise_error = TRUE),
        path,
        character()
    )
}

get_output_dir <- function(raise_error = TRUE) {
    conf <- config::get(file = get_dirconfig())
    path <- find_path_relative_root(conf$output_dir_basename)
    ifelse(
        test_path(path, raise_error = TRUE),
        path,
        character()
    )
}

get_scr_dir <- function(raise_error = TRUE) {
    conf <- config::get(file = get_dirconfig())
    path <- find_path_relative_root(conf$scr_dir_basename)
    ifelse(
        test_path(path, raise_error = TRUE),
        path,
        character()
    )
}

get_book_filename <- function(name = "_bookdown.yml", raise_error = TRUE) {
    conf <- config::get(file = get_dirconfig())
    path <- find_path_relative_root(conf$scr_dir_basename, name)
    ifelse(
        test_path(path = path, raise_error = raise_error),
        yaml::read_yaml(path)$book_filename,
        character()
    )
}

get_rendered_book <- function(extension = "html", raise_error = TRUE) {
    names <- stringr::str_replace(
        string = get_book_filename(),
        pattern = "\\.Rmd",
        replacement = paste0("\\.", extension)
    )
    # path of the file with that name
    path <- list.files(
        path = get_output_dir(),
        pattern = names[1],
        full.names = TRUE
    )[1]
    ifelse(
        test_path(path, raise_error),
        path,
        character()
    )
}

#
#
#
# we don't use the following script for this project.
#
#
#
to_abs_path <- function(root, relative_path_to_root) {
    withr::with_dir(root, normalizePath(relative_path_to_root) %>% return())
}

# search recursively.
# maximum depth of recursion is determined by depth argument.
get_vscode_workspace <- function(path = getwd(), depth = 2, path_init = NULL) {
    files <- list.files(path, pattern = "[a-zA-Z0-9_].code-workspace")
    if (length(files) > 0) {
        relative_from_code_workspace <- jsonlite::read_json(
            path = file.path(path, files[1])
        )$folders[[1]]$path
        # get the absolute path that *.code-workspace designates
        return(to_abs_path(path, relative_from_code_workspace))
    }
    # path_init only used for failure message
    path_init <- ifelse(path_init %>% is.null(), path, path_init)
    if (depth <= 0) {
        paste0(
            "No workspace file found.",
            "\n",
            "The search started: ",
            path_init,
            "\n",
            "And ended: ",
            path
        ) %>% stop()
    }
    # search parent folder
    return(get_vscode_workspace(dirname(path), depth - 1, path_init))
}
