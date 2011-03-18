##
##  Savitzky-Golay Smoothing
##  Pseudoinverse
##


savgol <- function(T, fl, forder=4, dorder=0)
{
    # -- calculate filter coefficients --
    fc <- (fl-1)/2                          # index: window left and right
    X <- outer(-fc:fc, 0:forder, FUN="^")   # polynomial terms and coeffs
    Y <- pinv(X);                           # pseudoinverse

    # -- filter via convolution and take care of the end points --
    T2 <- convolve(T, rev(Y[(dorder+1),]), type="o")   # convolve(...)
    T2 <- T2[(fc+1):(length(T2)-fc)]

    return( (-1)^dorder * T2 )
}

pinv <- function (A)
{
    s <- svd(A)
    # D <- diag(s$d); Dinv <- diag(1/s$d)
    # U <- s$u; V <- s$v
    # A = U D V'
    # X = V Dinv U'
    s$v %*% diag(1/s$d) %*% t(s$u)
}
