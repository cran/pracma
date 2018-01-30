##
##  p o i s s o n 2 d i s k . R
##


poisson2disk <- function(n, a = 1, b = 1, m = 10, info = TRUE) {
    if (a <= 0 || b <= 0)
        stop("Width a and height b must be positive reals.")
    if (floor(n) != ceiling(n) || n < 1 ||
        floor(m) != ceiling(m) || m < 1)
        stop("n and m must be integer numbers.")

    ab <- rep(c(a, b), times = c(m, m))
    A <- matrix(0, n, 2)
    A[1, ] <- c(a, b) * runif(2)

    i <- 2
    while (i <= n) {
        B <- matrix( ab * runif(2*m), nrow = m, ncol = 2)
        C <- distmat(B, A[1:(i-1), ])
        k <- which.max(apply(C, 1, min))
        A[i, ] <- B[k, ]
        i <- i + 1
    }
    if (info) {
        AA <- distmat(A, A)
        diag(AA) <- max(AA)
        d <- sqrt(2*a*b / n)
        cat("Minimal Distance between points: ", min(AA), '\n')
    }
    return(A)
}

