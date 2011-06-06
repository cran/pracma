##
##  n u m d e r i v . R  Richardson Numerical Derivative
##


numderiv <- function(f, x0, maxiter = 16, h = 1, ...,
                        accuracy = .Machine$double.eps)
{
    if (length(x0) != 1 || !is.numeric(x0))
        stop("Argument 'x0' must be a numeric scalar.")

    fun <- match.fun(f)
    f <- function(x) fun(x, ...)
    if (length(f(x0)) != 1)
        stop("Function 'f' must be a univariate function of one variable.")

    tol <- accuracy^(2/3)
    eps <- .Machine$double.eps
    err <- 1; relerr <- 1

    j <- 1
    D <- matrix(0, nrow = maxiter, ncol = maxiter)

    D[1, 1] <- (f(x0+h) - f(x0-h))/(2*h)
    while (relerr > tol && err > tol && j < maxiter) {
        h <- h/2
        D[j+1, 1] <- (f(x0+h) - f(x0-h))/(2*h)
        for (k in 1:j) {
            D[j+1, k+1] <- D[j+1, k] + (D[j+1,k] - D[j,k]) / (4^k - 1)
        }
        err <- abs(D[j+1,j+1] - D[j,j])
        relerr <- 2*err / (abs(D[j+1,j+1]) + abs(D[j,j]) + eps)
        j <- j + 1
    }
    if (j >= maxiter)
        warning("Maximum number of iterations reached.")

    return(list(df = D[j, j], err = err, relerr = relerr, n = j))
}


numdiff <- function(f, x, maxiter = 16, h = 1, ...,
                    accuracy = .Machine$double.eps) {
    if (!is.vector(x, mode = "numeric"))
        stop("")

    ndf <- function(xx) numderiv(f, xx, maxiter = maxiter, h = h, ...,
                                 accuracy = .Machine$double.eps)$df

    sapply(x, ndf)
}
