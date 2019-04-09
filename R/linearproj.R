##
##  l i n e a r p r o j . R  Linear and affine projection
##


linearproj <- function(A, B) {
    # Return the projection of points in the columns of B
    # onto the linear subspace spaned by the columns of A.
    stopifnot(is.numeric(A), is.numeric(B))
    if (!is.matrix(A)) A <- as.matrix(A)
    if (!is.matrix(B)) B <- as.matrix(B)
    nA <- nrow(A); mA <- ncol(A)
    nB <- nrow(B); mB <- ncol(B)
    if (nA != nB)  # dimension of R^n
        stop("Arguments 'A', 'B' must have the same number of rows.")
    if (Rank(A) < mA)
        stop("Matrix 'A' does not have maximal rank -- not a basis.")

    P  <- qr.solve(t(A) %*% A, t(A) %*% B)
    Q <- A %*% P
    return(list(P = P, Q = Q))
}


affineproj <- function(x0, C, b, unbound = TRUE, maxniter = 100) {
    if (unbound) {
        # Return projection of x0 onto the affine subspace 
        # C x = b and the distance of x0 from this subspace.
        mC <- nrow(C); nC <- ncol(C)    # n dimension, m codimension
        CC <- C %*% t(C)
        xp <- (diag(1, nC) - t(C) %*% qr.solve(CC, C)) %*% x0 + 
               t(C) %*% qr.solve(CC, b)
        d <- Norm(xp)
        return(list(proj = xp, dist = d, niter = 0))
    } else {
        # C fast projection method for enforcing equality and
        # positivity constraints: C x = b and x >= 0
        svdC <- svd(C); x <- x0
        svdmat <- svdC$v %*% diag(1/svdC$d, length(svdC$d)) %*% t(svdC$u)
        iterate <- TRUE; niter <- 0
        while(iterate & niter <= maxniter) { 
            niter <- niter + 1
            bCx <- (b - c(C %*% x))
            x <- x + c(svdmat %*% bCx)
            if (any(x < 0)) x[x < 0] <- 0 else iterate <- FALSE
        }
        d <- Norm(x0 - x)
        return(list(proj = x, dist = d, niter = niter))
    }
}
