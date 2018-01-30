##
##  s h u b e r t . R  Shubert-Piyawskii Method
##


shubert <- function(f, a, b, L, crit = 1e-04, nmax = 1000) {
    stopifnot(is.numeric(a), is.numeric(b),
              length(a) == 1, length(b) == 1)
    fun <- match.fun(f); f <- function(x) fun(x)

    nf <- 0
    x0 <- (a + b)/2.0
    y0 <- f(x0); nf <- nf + 1
    xmax <- x0; ymax <- y0
    fmax <- y0 + L*(b - a)/2.0

    X <- numeric(nmax); Z <- numeric(nmax)
    X[1] <- b; Z[1] <- y0 + fmax
    X[2] <- a; Z[2] <- y0 + fmax

    n <- 2
    while ((fmax - ymax) > crit && n < nmax) {
        xn <- X[n]; zn <- Z[n]
        yn <- f(xn); nf <- nf + 1
        if (yn > ymax) {
            xmax <- xn; ymax <- yn
        }
        zL <- (zn + yn)/2.0; zR <- zL
        xL <- xn - (zn - yn)/2.0/L
        xR <- xn + (zn - yn)/2.0/L

        i1 <- 0; i2 <- 0
        if (xL >= a && xL <= b) i1 <- 1
        if (xR >= a && xR <= b) i2 <- 1

        if (i1 == 1 && i2 == 0) {
            X[n] <- xL; Z[n] <- zL
        } else if (i1 == 0 && i2 == 1) {
            X[n] <- xR; Z[n] <- zR
        } else if (i1 == 1 && i2 == 1) {
            X[n] <- xL; Z[n] <- zL
            X[n+1] <- xR; Z[n+1] <- zR
            n <- n + 1
        }
        Zo <- order(Z[1:n])
        Z[1:n] <- Z[Zo]; X[1:n] <- X[Zo]
        fmax <- Z[n]
    }
    return(list(xopt = xmax, fopt = fmax, nopt = n))
}