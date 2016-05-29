###
### p o l y f i t . R  Polynom
###

polyfit <- function(x, y, n = 1) {
    if (!is.numeric(x) || !is.numeric(y))
        stop("Arguments x and y must be numeric.")
    if (length(x) != length(y))
        stop("Vectors/matrices x and y must be of same length.")
    if (is.null(n) || n < 0 || ceiling(n) != floor(n))
        stop("Degree n must be a non-negative integer.")

    x <- x[1:length(x)]; y <- y[1:length(y)]
    A <- outer(x, seq(n, 0), "^")
    p <- qr.solve(A, y)
    return(p)
}


polyfix <- function(x, y, n, xfix, yfix)
{
    stopifnot(is.numeric(x), is.numeric(y), is.numeric(xfix), is.numeric(yfix))
    nfit = length(x)
    if (length(y) != nfit)
        stop("Arguments 'x' and 'y' must have the same length.")
    nfix = length(xfix)
    if (length(yfix) != nfix)
        stop("Arguments 'xfix' and 'yfix' must have the same length.")

    if (!is.numeric(n) || n < 0 || floor(n) != ceiling(n))
        stop("Argument 'n' must be an integer.")
    if (n < nfix-1) {
        stop("Argument 'n' shall be greater or equal to 'nfix-1'.")
    } else if (n == nfix-1) {
        warning("Data points can not be taken into account.")
        return(polyfit(xfix, yfix, n))
    }

    A <- ones(nfix, n+1)
    for (i in 1:(n+1))
        A[, i] <- ones(nfix, 1) * xfix^(n+1-i)

    pc <- qr.solve(A[,(n+2-nfix):ncol(A)], yfix)  # Satifies A*pc = yfix

    pcfull <- matrix(0, n+1, 1)
    pcfull[(n+2-nfix):(n+1)] <- pc

    yfit <- y - polyval(pc, x)

    B <- nullspace(A)
    X <- matrix(0, nfit,n+1)
    for(i in 1:(n+1))
        X[, i] <- matrix(1, nfit,1) * x^(n+1-i)

    z = qr.solve(X %*% B, yfit)  # Least squares solution of X*B*z = yfit
    p0 <- B %*% z                # Satisfies A*p0 = 0
    p <- p0 + pcfull             # Satisfies A*p = b

    return(c(p))
}


# polyfit2 <- function(x, y, n = 1, p0 = NULL) {
#     if (!is.numeric(x) || !is.numeric(y))
#         stop("Arguments 'x' and 'y' must be numeric.")
#     if (length(x) != length(y))
#         stop("Vectors/matrices 'x' and 'y' must be of same length.")
#     if (is.null(n) || n <= 0 || ceiling(n) != floor(n))
#         stop("Argument 'n', order of fit, must be a positive integer.")
#     if (is.null(p0))
#         return(polyfit(x, y, n = n))
#     else if (!is.numeric(p0) || length(p0) != 2)
#         stop("Argument 'p0' must be a numeric vector of length 2.")
# 
#     x0 <- p0[1];  y0 <- p0[2]
#     xx <- x - x0; yy <- y - y0
# 
#     M <- matrix(0, length(x), n)
#     M[, n] <- xx
#     for (i in (n-1):1) {
#         M[, i] <- xx * M[, i+1]
#     }
#     pt <- qr.solve(M, yy)
#     pt <- c(pt, y0)
#     p <- numeric(n+1)
#     for (i in (n+1):1) {
#         p[i] <- polyval(pt, -x0)
#         pt   <- polyder(pt)/(n-i+2)
#     }
#     return(p)
# }
