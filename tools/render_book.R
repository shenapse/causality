library(magrittr)
# specifies the file project-root/tools/tools.R
source(rprojroot::is_git_root$make_fix_file()("tools", "tools.R"))

conf <- config::get(file = get_dirconfig())
# create output directory if it does not exists
if (get_output_dir() %>% dir.exists() %>% isFALSE()) {
    dir.create(get_output_dir())
}
# create empty book file if it does not exist. this is just for convenience.
# get book file path
book_file_path <- find_path_relative_root(
    conf$scr_dir_basename,
    get_book_filename()
)
if (book_file_path %>% file.exists() %>% isFALSE()) {
    file.create(book_file_path)
}

withr::with_dir(
    new = get_scr_dir(),
    code = bookdown::render_book(
        input = conf$rmd_basename,
        output_dir = get_output_dir()
    )
)
