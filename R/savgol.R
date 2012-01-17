##
##  Savitzky-Golay Smoothing
##  Pseudoinverse (Moore-Penrose Generalized Inverse)
##


savgol <- function(T, fl, forder=4, dorder=0) {
    stopifnot(is.numeric(T), is.numeric(fl))
    if (fl <= 1 || fl %% 2 == 0)
        stop("Argument 'fl' must be an odd integer greater than 1.")
    n <- length(T)

    # -- calculate filter coefficients --
    fc <- (fl-1)/2                          # index: window left and right
    X <- outer(-fc:fc, 0:forder, FUN="^")   # polynomial terms and coeffs
    Y <- pinv(X);                           # pseudoinverse

    # -- filter via convolution and take care of the end points --
    T2 <- convolve(T, rev(Y[(dorder+1),]), type="o")   # convolve(...)
    T2 <- T2[(fc+1):(length(T2)-fc)]

    Tsg <- (-1)^dorder * T2
    return( Tsg )
}

pinv <- function (A, tol=.Machine$double.eps^(2/3)) {
    stopifnot(is.numeric(A), length(dim(A)) == 2, is.matrix(A))

    s <- svd(A)
    # D <- diag(s$d); Dinv <- diag(1/s$d)
    # U <- s$u; V <- s$v
    # A = U D V'
    # X = V Dinv U'

    p <- ( s$d > max(tol * s$d[1], 0) )
    if (all(p)) {
        mp <- s$v %*% diag(1/s$d) %*% t(s$u)
    } else if (any(p)) {
        mp <- s$v[, p, drop=FALSE] %*% diag(1/s$d[p]) %*% t(s$u[, p, drop=FALSE])
    } else {
        mp <- matrix(0, nrow=ncol(A), ncol=nrow(A))
    }

    return(mp)
}
