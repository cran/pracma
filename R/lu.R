##
##  l u. R  LU Decomposition
##


lu <- function(A, scheme = c("kji", "jki", "ijk")) {
    stopifnot(is.numeric(A), is.matrix(A))
    n <- nrow(A)
    if (ncol(A) != n || n <= 1)
        stop("Argument 'A' must be a square, positive definite matrix.")

    scheme <- match.arg(scheme)

    if (scheme == "kji") {
        for (k in 1:(n-1)) {
            if (A[k, k] == 0)
                stop("All diagonal elements of matrix 'A' must be non-zero.")
            for (i in (k+1):n) {
                A[i, k] <- A[i, k] / A[k, k]
                A[i, (k+1):n] <- A[i, (k+1):n] - A[i, k] * A[k, (k+1):n]
            }
        }

    } else if (scheme == "jki") {
        if (A[1, 1] == 0)
            stop("All diagonal elements of matrix 'A' must be non-zero.")
        i <- 2:n
        A[i, 1] <- A[i, 1] / A[1, 1]
        for (j in 2:n) {
            if (A[j, j] == 0)
                stop("All diagonal elements of matrix 'A' must be non-zero.")
            for (k in 1:(j-1)) {
                i <- (k+1):n
                A[i, j] <- A[i, j] - A[i, k] * A[k, j]
            }
            if (j < n) {
                i <- (j+1):n
                A[i, j] <- A[i, j] / A[j, j]
            }
        }

    } else if (scheme == "ijk") {       # 'compact' Doolittle scheme
        for (i in 2:n) {                # j in 1:n
            for (j in 2:i) {
                if (A[j, j] == 0)
                    stop("All diagonal elements of matrix 'A' must be non-zero.")
                A[i, j-1] <- A[i, j-1] / A[j-1, j-1]
                k <- 1:(j-1)
                A[i, j] <- A[i, j] - A[i, k] %*% A[k, j]
            }
            if (i < n) {
                k <- 1:(i-1)
                for (j in (i+1):n) {
                    A[i, j] <- A[i, j] - A[i, k] %*% A[k, j]
                }
            }
        }
    }

    L <- eye(n) + tril(A, -1)
    U <- triu(A)
    return(list(L = L, U = U))
}

