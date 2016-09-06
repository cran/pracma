##
##  f p r i n t f . R  Formatted printing to stdout or file
##


fprintf <- function(fmt, ..., file = "", append = FALSE) {
    mystr <- sprintf(fmt, ...)
    cat(mystr, file = file, append = append)
    invisible(nchar(mystr))
}
