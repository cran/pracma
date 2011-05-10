##
##  m r a n k . R  Matrix Rank
##


mrank <- function(M) {
    if (length(M) == 0)
        return(0)
    if (!is.numeric(M))
        stop("Argument 'M' must be a numeric matrix.")
    if (is.vector(M))
        M <- matrix(c(M), nrow = length(M), ncol = 1)

    # The MASS way
    r1 <- qr(M)$rank

    # The Matlab way
    sigma <- svd(M)$d
    tol <- max(dim(M)) * max(sigma) * .Machine$double.eps
    r2 <- sum(sigma > tol)

    if (r1 != r2)
        warning("Rank calculation may be problematic.")
    return(r2)
}


nullspace <- function(M) {
    if (!is.numeric(M))
        stop("Argument 'M' must be a numeric matrix.")
    if (is.vector(M))
        M <- matrix(c(M), nrow = length(M), ncol = 1)

    qrM <- qr(M)
    rank <- qrM$rank
    if (rank == ncol(M)) return(rep(0, ncol(M)))

    inds <- if (rank == 0) 1:ncol(M) else -(1:rank)
    qrQ <- qr.Q(qrM, complete = TRUE)[, inds, drop = FALSE]

    if (length(qrQ) == 0)
        return(rep(0, ))
    return(t(qrQ))
}
