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


who <- function() ls(name = .GlobalEnv)


what <- function(dname = getwd()) {  # , fexp = "*.R"
    if (is.na(file.info(dname)$isdir)) {
        cat("Argument '", dname, "' is not a known directory.\n", sep = '')
    } else if (file.info(dname)$isdir) {
        fnames <- list.files(dname)
        cat("Files in Directory ", dname, ":\n\n", sep = '')
        for (fname in fnames) {
            gname <- paste(dname, fname, sep = "/")
        	if (!file.info(gname)$isdir) {
        	    cat(fname, "\n")
        	}
        }
    } else {
        cat("Argument '", dname, "' is not a directory.\n", sep = '')
    }
    invisible(NULL)
}
