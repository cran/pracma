##
##  d b l q u a d . R  Double Integration
##


dblquad <- function(f, xa, xb, ya, yb, dim = 2, ..., 
                    subdivs = 300, tol = .Machine$double.eps^0.5) {
    stopifnot(is.numeric(xa), length(xa) == 1, is.numeric(xb), length(xb) == 1,
              is.numeric(ya), length(ya) == 1, is.numeric(yb), length(yb) == 1)

    fun <- match.fun(f)
    f <- function(x, y) fun(x, y, ...)
    if (length(f(c(xa, xb), c(ya, yb))) != 2)
        stop("Function 'f' does not appear to be vectorized.")

    if (dim == 2) {
        fy <- function(x)
                integrate(function(y) f(x, y), ya, yb,
                          subdivisions = subdivs, rel.tol = tol)$value
        Fy <- Vectorize(fy)
        Q  <- integrate(Fy, xa, xb,
                        subdivisions = subdivs, rel.tol = tol)$value

    } else if (dim == 1) {
        fx <- function(y)
                integrate(function(x) f(x, y), xa, xb,
                          subdivisions = subdivs, rel.tol = tol)$value
        Fx <- Vectorize(fx)
        Q  <- integrate(Fx, ya, yb,
                        subdivisions = subdivs, rel.tol = 10*tol)$value

    } else
        stop("Argument 'dim' can only be 1 (x-) or 2 (y-variable first).")

    return(Q)
}


triplequad <- function(f, xa, xb, ya, yb, za, zb, 
                        subdivs = 300, tol = .Machine$double.eps^0.5, ...) {
    stopifnot(is.numeric(xa), length(xa) == 1, is.numeric(xb), length(xb) == 1,
              is.numeric(ya), length(ya) == 1, is.numeric(yb), length(yb) == 1,
              is.numeric(za), length(za) == 1, is.numeric(zb), length(zb) == 1)

    fun <- match.fun(f)
    f <- function(x, y, z) fun(x, y, z, ...)

    fyz <- function(y, z) {
        Qin <- numeric(length(y))
        for (i in 1:length(y)) {
            fx  <- function(x) f(x, y[i], z[i])
            Qin <- integrate(fx, xa, xb,
                             subdivisions = subdivs, rel.tol = 1e-10)$value
        }
        Qin
    }
    fyz <- Vectorize(fyz)
    dblquad(fyz, ya, yb, za, zb, tol = tol)
}
