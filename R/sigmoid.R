##
##  s i g m o i d . R  Sigmoid Function
##


sigmoid <- function(x, a = 1, b = 0) {
    if (length(x) == 0) return(c())
    stopifnot(is.numeric(x) || is.complex(x))
    stopifnot(is.numeric(a), is.numeric(b))
    a <- a[1]; b <- b[1]

    1 / (1 + exp(-a*(x-b)))
}
