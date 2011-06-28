##
##  q u a d c c . R  Clenshaw-Curtis Quadrature
##


clenshaw_curtis <- function(f, a = -1, b = 1, n = 32, ...) {
    fun <- match.fun(f)
    f <- function(x) fun(x, ...)

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
