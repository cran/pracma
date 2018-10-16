hessenberg <- function(A) {
    stopifnot(is.numeric(A), is.matrix(A))
    m <- nrow(A); n <- ncol(A)
    if (m != n)
        stop("Input 'A' must be a square matrix.")
    if (n <= 2) {
        return(list(H = A, P = eye(n)))
    }

    # initialize variables
    H <- A
    V <- vector(mode = "list", length = n-2)

    # Householder transformation
    for (k in 1:(n-2)) {
        v <- H[(k+1):n, k]
        sgn <- sign(v[1])
        if (sgn == 0) sgn <- 1
        v[1] <- v[1] + sgn * Norm(v)
        if (Norm(v) != 0) v <- v / Norm(v)

        H[(k+1):n,k:n] <- H[(k+1):n, k:n] -
                          2 * v %*% (t(v) %*% H[(k+1):n,k:n])
        H[ ,(k+1):n] <- H[ , (k+1):n] -
                        (2 * (H[ , (k+1):n] %*% v)) %*% t(v)
        V[[k]] <- v
    }

    Q <- eye(n)
    for (j in (n-2):1) {
        Q[(j+1):n, ] <- Q[(j+1):n, ] -
                       (2 * V[[j]]) %*% (t(V[[j]]) %*% Q[(j+1):n, ])
    }

    return(list(H = H, P = Q))
}