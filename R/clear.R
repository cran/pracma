##
##  c l e a r . R
##


clear <- function(lst) {
    if (missing(lst))
        lst <- ls(name = .GlobalEnv)
    rm(list = lst, envir = globalenv())
    # capture.output(gc())
    null <- gc()
}
