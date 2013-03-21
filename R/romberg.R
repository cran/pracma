##
##  r o m b e r g . R  Romberg Integration
##


romberg <- function(f, a, b, maxit = 50, tol = 1e-6, ...)
{
    stopifnot(is.numeric(a), is.numeric(b), length(a) == 1, length(b) == 1)

    fun <- match.fun(f)
    f <- function(x) fun(x, ...)

    if (a == b) return(list(value = 0, rel.error = 0))
    eps <- .Machine$double.eps  # assume a < b
    if (!is.finite(f(a))) a <- a + eps * sign(b-a)
    if (!is.finite(f(b))) b <- b - eps * sign(b-a)

    n <- 1
    I <- matrix(0, nrow = maxit+1, ncol = maxit+1)
    iter <- 0
    while (iter < maxit) {
        iter <- iter+1
        n <- 2^iter
        I[iter+1, 1] <- .trap(f, a, b, n)
        for (k in 2:(iter+1)) {
            j <- 2+iter-k
            I[j,k] <- (4^(k-1)*I[j+1,k-1] - I[j,k-1]) / (4^(k-1)-1)
        }
        err <- abs((I[1,iter+1] - I[2,iter]) / I[1,iter+1])
        if (err < tol) break
    }
    if (iter == maxit)
        warning("Maximum number of iterations has been reached.")

    return(list(value = I[1, iter+1], iter = iter, rel.error = err))
}


.trap <- function(f, a, b, n) {
    h <- (b-a) / n
    S <- f(a)
    if (n > 1) {
        for (i in 1:(n-1)) { xi <- a + h * i; S <- S + 2*f(xi) }
    }
    return((S + f(b)) * h/2)
}
