# clear cache -> clear outputs -> clear cache
# specifies the file project-root/tools/your-script-in-tools.R
source(rprojroot::is_git_root$make_fix_file()("tools", "clear_cache.R"))
source(rprojroot::is_git_root$make_fix_file()("tools", "tools.R"))
# check that output_dir seems right.
# ok if no rmd file is included
if (list.files(get_output_dir(), "[a-zA-Z0-9_].Rmd", recursive = TRUE, ignore.case = TRUE) %>% length(.) > 0) {
    stop("Are you clearing a wrong directory? It has a Rmd file.")
}
args <- paste(".", get_output_dir(), "-x", "rm -rf {}")
base::system2(command = "fd", args = args)
source(rprojroot::is_git_root$make_fix_file()("tools", "clear_cache.R"))
