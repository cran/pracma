##
##  a r n o l d i . R  Arnoldi Iteration
##

arnoldi <- function(A, q, m) {
    stopifnot(is.numeric(A), is.numeric(q))
    if (!is.matrix(A) || nrow(A) != nrow(A))
        stop("Argument 'A' must be a square matrix")
    n <- nrow(A)
    q1 <- as.matrix(c(q))
    if (length(q1) != n)
        stop("Argument 'q' must be a vector of length 'nrow(A)'.")
    if (missing(m)) m <- n

    q1 <- q1 / Norm(q1)
    Q <- zeros(n,m)
    Q[, 1] <- q1
    H <- zeros(min(m+1,m), n)

    for (k in 1:m) {
        z <- A %*% Q[, k]
        for (i in 1:k) {
            H[i, k] <- t(Q[, i]) %*% z
            z <- z - H[i, k] * Q[, i]
        }
        if (k < n) {
            H[k+1, k] <- Norm(z)
            if (H[k+1, k] == 0)
                return(list(Q = Q, H = H))
            Q[, k+1] <- z / H[k+1, k]
        }
    }
    return(list(Q = Q, H = H))
}
