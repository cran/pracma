##
##  m d i a g . R  Matrix diagonal
##


mdiag <- function(x, k=0) {
    if (!is.numeric(x) && !is.complex(x))
        stop("Argument 'x' must be a real or complex vector or matrix.")
    if (!is.numeric(k) || k != round(k))
        stop("Argument 'k' must be an integer.")

    if (length(x) == 1) return(x)
    if (is.matrix(x)) {
        n <- nrow(x); m <- ncol(x)
        if (k >= m || -k >= n) {
            y <- matrix(0, nrow=0, ncol=0)
        } else {
            y <- x[col(x) == row(x) + k]
        }
    } else {
        if (is.vector(x)) {
            n <- length(x)
            m <- n + abs(k)
            y <- matrix(0, nrow=m, ncol=m)
            y[col(y) == row(y) + k] <- x
        } else {
            stop("Argument 'x' must be a real or complex vector or matrix.")
        }
    }
    return(y)
}


diffmat <- function(x){
    if (!is.vector(x, mode="numeric"))
        stop("Argument 'x' must be a numeric vector.")
    n <- length(x)
    if (n == 1) return(x)

    M <- matrix(nrow=n+1, ncol=n)
    for(i in 1:n){
        M[i, seq.int(along=x)] <- x
        x <- diff(x)
    }
    A <- matrix(M, nrow=n, ncol=n)
    A[upper.tri(A)] <- t(A)[upper.tri(A)]
    return(A)
}

# Reorganizing an (n+1) x n matrix into an n x n matrix
# shifts i-th column by (i-1) downwards. In particular,
# the first row becomes the main diagonal. The initial
# part of each of the remaining rows becomes a diagonal
# starting at the first component of the original row.
