##
##  c o n d . R  Matrix Condition
##


cond <- function(M, p = 2) {
    if (length(M) == 0)
        return(0)
    if (!is.numeric(M))
        stop("Argument 'M' must be a numeric matrix.")
    if (is.vector(M))
        M <- matrix(c(M), nrow = length(M), ncol = 1)

    if (length(M) == 0) return(c())
    if (ncol(M) != nrow(M) && p != 2)
        stop("Matrix 'M' must be square if p = 2.")

    if (p == 2) {
        s <- svd(M)$d
        cnd <- if (any(s == 0)) Inf else max(s) / min(s)
    } else {
        stop("At the moment, p-norms other than p = 2 are not implemented.")
        #cnd <- norm(M, p) * norm(inv(M), p)
    }
    return(cnd)
}


normest <- function(M, maxiter = 100, tol = .Machine$double.eps^(1/2)) {
    if (length(M) == 0)
        return(0)
    if (!is.numeric(M))
        stop("Argument 'M' must be a numeric matrix.")
    if (is.vector(M))
        M <- matrix(c(M), nrow = length(M), ncol = 1)

    x <- matrix(apply(abs(M), 2, sum), ncol = 1)
    est <- norm(x, "F")      # Frobenius Norm
    if (est == 0) return(0)

    x <- x/est
    est0 <- 0
    niter <- 0
    while (abs(est - est0) > tol * est && niter <= maxiter) {
        est0 <- est
        Mx <- M %*% x
        if (all(Mx == 0))
            Mx <- matrix(runif(length(Mx)), ncol(Mx), nrow(Mx))
        x <- t(M) %*% Mx
        normx <- norm(x, "F")
        est <- normx / norm(Mx, "F")
        x <- x / normx
        niter <- niter + 1
    }
    if (niter > maxiter)
        warning("Number of iterations exceeded 'maxiter'.")

    return(est)
}


