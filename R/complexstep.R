##
##  c o m p l e x s t e p . R  Complex Step Derivation
##


complexstep <- function(f, x0, h = 1e-20, test = FALSE, ...) {
    fun <- match.fun(f)
    f <- function(x) fun(x, ...)

    f_csd <- Im(f(x0 + h * 1i)) / h
    # Test it as analycity may fail for function f
    if (test) {
        h0 <- 1e-5  # 2 * .Machine$double.eps^(1/3)
        f_grd <- (f(x0+h0/2) - f(x0-h0/2))/ h0
        if (abs(f_csd - f_grd) > h0)
            warning(paste("Complex Step check failed: ", abs(f_csd - f_grd)))
        else
            cat("Derivation Difference: ", abs(f_csd - f_grd), "\n")
    }
    return(f_csd)
}


jacobiancsd <- function(f, x0, h = 1e-20, ...) {
    fun <- match.fun(f)
    f <- function(x) fun(x, ...)

    z <- f(x0)
    n <- length(x0); m <- length(z)
    J <- matrix(NA, nrow = m, ncol = n)

    for (k in 1:n) {
        x1 <- x0
        x1[k] <- x1[k] + h * 1i
        J[ , k] <- Im(f(x1)) / h
    }
    # drop matrix dimensions if n = m = 1
    if (m == 1 && n == 1) J <- J[,]
    return(J)
}
