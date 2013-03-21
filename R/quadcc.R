##
##  q u a d c c . R  Adaptive Clenshaw-Curtis Quadrature
##


.quadcc <- function(f, a, b, tol = .Machine$double.eps^0.5, ...) {
    stopifnot(is.numeric(a), is.numeric(b),
              length(a) == 1, length(b) == 1)

    fun <- match.fun(f)
    f <- function(x) fun(x, ...)

    eps <- .Machine$double.eps
    if (a == b) {
        return(list(value = 0, error = 0))
    } else if (a > b) {
        Q <- quadcc(f, b, a, tol = tol)
        return(list(value = -Q$value, error = Q$error))
    }

    # Break integration interval into four regions
    N <- 4
    p <- (b+a)/2
    q <- (b-a)/2
    # Write out [ cos((0:4)/4*pi) ] explicitly to avoid cos(pi/2) roundoff
    x <- p - q * c(1, 0.7071067811865475, 0, -0.7071067811865475, -1)
    F <- f(x)

    if (!is.finite(F[1])) F[1] <- f(x[1] + eps)
    if (!is.finite(F[5])) F[5] <- f(x[5] - eps)

    # Modify the first and last term of F
    F[1] <- F[1]/2
    F[5] <- F[5]/2

    y <- 0; yinit <- Inf
    while (abs(y - yinit) > tol && N < 2^13) {
        yinit <- y
        # Double the size of the approximation
        N <- 2*N
        Nvec <- 0:N

        x2 <- numeric(N+1)
        s1 <- seq(1, N+1, by = 2)
        s2 <- seq(2, N+1, by = 2)
        x2[s1] <- x
        x2[s2] <- p - q * cos(seq(1,N,by=2)/N * pi)

        G <- numeric(N+1)
        G[s1] <- F
        G[s2] <- f(x2[s2])

        if (any(is.infinite(G))) {
            warning("Integrand has a pole between integration limits.")
            y <- NA
            break
        }

        # Evaluate the Chebyshev polynomial coefficients
        M <- cos(as.matrix(Nvec[seq(1, length(Nvec)-1, by=2)]) %*% Nvec/N*pi)
        M[abs(M) < eps] <- 0
        C <- 4/N * M %*% G

        # Compute the quadrature coefficients
        coeffs <- -1/Nvec[s2]/(Nvec[s2]-2)
        coeffs[1] <- 0.5

        # Compute the integral estimate
        y <- q * sum(coeffs * C)

        # Reset the variables
        x <- x2
        F <- G
    }
    return(list(value = y, error = abs(y - yinit)))
}
