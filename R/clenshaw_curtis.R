##
##  q u a d c c . R  Clenshaw-Curtis Quadrature
##


clenshaw_curtis <- function(f, a = -1, b = 1, n = 1024, ...) {
    fun <- match.fun(f)
    f <- function(x) fun(x, ...)

    if (a == b) return(0)
    eps <- .Machine$double.eps  # assume a < b
    if (!is.finite(f(a))) a <- a + eps * sign(b-a)
    if (!is.finite(f(b))) b <- b - eps * sign(b-a)

    # Evaluate f at Chebyshev points
    x  <- cos(pi*(0:n)/n)
    fx <- f(0.5*((b-a)*x + (b+a)))/(2*n)

    # Fast Fourier transform
    g  <- Re(fft(fx[c(1:(n+1), n:2)]))

    # Chebyshev coefficients and weight vector
    d  <- c(g[1], g[2:n] + g[(2*n):(n+2)], g[n+1])
    w <- 0 * d
    w[seq(1, n+1, by=2)] <- 2/(1-(seq(0, n, by=2))^2)

    # Return the integral
    Q <- sum(w * d) * (b-a)/2
    return(Q)
}


quadcc <- function(f, a, b, tol = .Machine$double.eps^0.5, ...) {
    stopifnot(is.numeric(a), length(a) == 1,
              is.numeric(b), length(b) == 1)
    eps <- .Machine$double.eps

    fun <- match.fun(f)
    f <- function(x) fun(x, ...)
    
    if (a == b)     return(0)
    else if (a > b) return(-1 * quadgk(f, b, a, tol = tol))
    if (!is.finite(f(a))) a <- a + eps * sign(b-a)
    if (!is.finite(f(b))) b <- b - eps * sign(b-a)

    .ccadpt <- function(f, a, b, tol = tol) {
        Q4 <- clenshaw_curtis(f, a, b, n = 4)
        Q8 <- clenshaw_curtis(f, a, b, n = 8)
        if (abs(Q4 - Q8) < tol) {
            return(Q8)
        } # else

        Q2 <- .ccadpt(f, (a+b)/2, b, tol = tol)
        Q1 <- .ccadpt(f, a, (a+b)/2, tol = tol)

        return(Q1 + Q2)
    }
    return(.ccadpt(f, a, b, tol))
}
