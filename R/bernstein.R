bernstein <- function(f, n, x) {
    stopifnot(is.function(f), is.numeric(n), is.numeric(x))
    if (floor(n) != ceiling(n) || n < 0)
        stop("Argument 'n' must be a nonnegative integer.")
    if (any(x < -1.0 || x > 1.0))
        stop("Elements of argument 'x' must all lie in [0, 1].")

    if (length(x) == 1) {
        b2 <- f((0:n)/n)
        for (j in 1:n) {
            b1 <- b2
            b2[1:(n-j+1)] <- b1[1:(n-j+1)] * (1-x) + b1[2:(n-j+2)] * x
        }
        b <- b2[1]
    } else {
        b <- 0
        for (k in 0:n) {
            b <- b + choose(n,k) * x^k * (1-x)^(n-k) * f(k/n)
        }
    }
    return(b)
}


bernsteinb<- function(k, n, x) {
    stopifnot(is.numeric(k), is.numeric(n), is.numeric(x))
    if (floor(n) != ceiling(n) || n < 0)
        stop("Argument 'n' must be a nonnegative integer.")
    if (floor(k) != ceiling(k) || k < 0 || k > n)
        stop("Argument 'k' must be an integer between 0 and 'n'.")
    if (any(x < -1.0 || x > 1.0))
        stop("Elements of argument 'x' must all lie in [0, 1].")

    choose(n,k) * x^k * (1-x)^(n-k)
}
