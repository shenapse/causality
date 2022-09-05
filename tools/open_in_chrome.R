# specifies the file project-root/tools/tools.R
source(rprojroot::is_git_root$make_fix_file()("tools", "tools.R"))

# stop if output file is not found
if (get_rendered_book() %>% file.exists() %>% isFALSE()) {
    stop(
        "No rendered results found. bookdown::render_book() first."
    )
}
base::system2("google-chrome", args = get_rendered_book())
