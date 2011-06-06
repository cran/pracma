##
##  h e s s i a n . R  Hessian Matrix
##


hessian <- function(f, x0, h = 1e-4, ...) {
    if (!is.numeric(x0))
        stop("Argument 'x0' must be a numeric value.")

    fun <- match.fun(f)
    f <- function(x) fun(x, ...)

    n <- length(x0)
    if (length(f(x0)) != 1)
        stop("Function 'f' must be a univariate function of n variables.")

    if (n == 1)
        return(matrix(fderiv(f, x0, n = 2, h = h), nrow = 1, ncol = 1))

    H <- matrix(NA, nrow = n, ncol = n)
    hh <- diag(h, n)
    for (i in 1:(n-1)) {
        hi <- hh[, i]
        H[i, i] <- (f(x0-hi) - 2*f(x0) + f(x0+hi)) / h^2
        for (j in (i+1):n) {
            hj <- hh[, j]
            H[i, j] <- (f(x0+hi+hj) - f(x0+hi-hj) - f(x0-hi+hj) + f(x0-hi-hj)) / (4*h^2)
            H[j, i] <- H[i, j]
        }
    }
    hi <- hh[, n]
    H[n, n] <- (f(x0-hi) - 2*f(x0) + f(x0+hi)) / h^2

    return(H)
}


laplacian <- function(f, x0, h = 1e-4, ...) {
    if (length(x0) != 2 || !is.numeric(x0))
        stop("Argument 'x0' must be a numeric vector of two elements.")

    fun <- match.fun(f)
    f <- function(x) fun(x, ...)

    if (length(f(x0)) != 1)
        stop("Function 'f' must be a univariate function of 2 variables.")

    h1 <- c(h, 0); h2 <- c(0, h)
    L <- (-4*f(x0) + f(x0+h1) + f(x0-h1) + f(x0+h2) + f(x0-h2)) / h^2
    return(L)
}
