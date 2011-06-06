##
##  g r a d . R  Function Gradient
##


grad <- function(f, x0, h = 1e-4, ...) {
    if (!is.numeric(x0))
        stop("Argument 'x0' must be a numeric value.")

    fun <- match.fun(f)
    f <- function(x) fun(x, ...)

    if (length(f(x0)) != 1)
        stop("Function 'f' must be a univariate function of 2 variables.")
    n <- length(x0)

    hh <- rep(0, n)
    gr <- numeric(n)
    for (i in 1:n) {
        hh[i] <- h
        gr[i] <- (-f(x0+2*hh) + 8*f(x0+hh) - 8*f(x0-hh) + f(x0-2*hh)) / (12*h)
        hh[i] <- 0
    }
    return(gr)
}
