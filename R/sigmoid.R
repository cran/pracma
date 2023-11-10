##
##  s i g m o i d . R  Sigmoid Function
##


sigmoid <- function(x, a = 1, b = 0) {
    if (length(x) == 0) return(c())
    stopifnot(is.numeric(x), is.numeric(a), is.numeric(b))
    a <- a[1]; b <- b[1]

    1 / (1 + exp(-a*(x-b)))
}


logit <- function (x, a = 1, b = 0) {
    if (length(x) == 0) 
        return(c())
    stopifnot(is.numeric(x), is.numeric(a), is.numeric(b))
    a <- a[1]; b <- b[1]
    if (any(x < 0) | any(x > 1))
        warning("NaNs produced with x < 0 or x > 1.", call. = TRUE)
    inds <- which(x < 0.0 | x > 1.0)
    x[inds] <- 0.5
    y <- b + log(x/(1 - x))/a
    y[inds] <- NaN

    return(y)
}
