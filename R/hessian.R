##
##  h e s s i a n . R  Hessian Matrix
##


hessian <- function(f, x0, h = .Machine$double.eps^(1/4), ...) {
    if (!is.numeric(x0))
        stop("Argument 'x0' must be a numeric vector.")

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


hessvec <- function (f, x, v, csd = FALSE, ...) {
    if (!is.numeric(x) || !is.numeric(v)) 
        stop("Arguments 'x' and 'v' must be a numeric.")
    fun <- match.fun(f)
    f <- function(x) fun(x, ...)
    if (length(x) < 2 || length(f(x)) != 1) 
        stop("Function 'f' must be univariate of 2 or more variables.")
    if (length(v) != length(x))
        stop("Vectors 'x' and 'v' must be of equal length.")
    
    h <- .Machine$double.eps^(1/4)
    r <- h / sqrt(sum(v*v))
    if (csd) {
        hv <- (grad_csd(f, x+r*v) - grad_csd(f, x-r*v))/(2*r)
    } else {
        hv <- (grad(f, x+r*v) - grad(f, x-r*v))/(2*r)
    }
    return(hv)
}


hessdiag <- function(f, x, ...) {
    if (!is.numeric(x)) 
        stop("Arguments 'x' and 'v' must be a numeric.")
    fun <- match.fun(f)
    f <- function(x) fun(x, ...)
    if (length(x) < 2 || length(f(x)) != 1) 
        stop("Function 'f' must be univariate of 2 or more variables.")
    
    n <- length(x)
    h <- .Machine$double.eps^(1/4)
    hh <- rep(0, n)
    HD <- numeric(n)
    for (i in 1:n) {
        hh[i] <- h
        HD[i] <- (f(x + hh) - 2*f(x) + f(x - hh)) / h^2
        hh[i] <- 0
    }
    return(HD)
}


laplacian <- function(f, x0, h = .Machine$double.eps^(1/4), ...) {
    if (!is.numeric(x0))
        stop("Argument 'x0' must be a numeric vector.")

    fun <- match.fun(f)
    f <- function(x) fun(x, ...)

	n  <- length(x0)
	hh <- rep(0, n)
	L  <- 0
	for (i in 1:n) {
		hh[i] <- h
		L <- L + (f(x0+hh) + f(x0-hh) - 2*f(x0)) / h^2
		hh[i] <- 0
	}

    return(L)
}
