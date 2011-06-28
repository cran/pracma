##
##  r o m b e r g . R  Romberg Integration
##


romberg <- function(f, a, b, tol = .Machine$double.eps^(2/3), ...)
{
    stopifnot(is.numeric(a), is.numeric(b), length(a) == 1, length(b) == 1)

    if (a == b) return(list(Q = 0, rel.error = 0, j = 0))

    fun <- match.fun(f)
    f <- function(x) fun(x, ...)

    if (!is.finite(f(a)) || !is.finite(f(b)))
        stop("Function 'f' is not finite at interval boundaries.")

    trap <- function(f, a, b, n) {
        h <- (b-a) / n
        S <- f(a)
        if (n > 1) {
            for (i in 1:(n-1)) { xi <- a + h * i; S <- S + 2*f(xi) }
        }
        return((S + f(b)) * h/2)
    }

    kmax <- 17
    Q <- matrix(0, nrow = kmax+1, ncol = kmax+1)
    k <- 1
    n <- 2^k
    Q[k+1, 1] <- trap(f, a, b, n)
    Q[1, 2]   <- (4*Q[2, 1] - Q[1, 1])/3

    for (k in 2:kmax) {
       n <- 2^k
       Q[k+1, 1] <- trap(f, a, b, n)
       for (j in 2:(k+1)) {
          c = 4^(j-1)
          Q[k-j+2, j]  = (c*Q[k-j+3, j-1] - Q[k-j+2, j-1]) / (c-1)  
       }
       rel.error <- abs(Q[1, k+1] - Q[1, k]) / Q[1, k+1]
       if (rel.error < tol) break
    }
    if (k == kmax)
        warning("Maximum number of iterations has been reached.")

    return(list(value = Q[1, j], rel.error = abs(rel.error)))
}
