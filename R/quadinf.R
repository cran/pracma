##
##  q u a d i n f . R  Infinite Integrals
##


quadinf <- function(f, xa, xb, tol = .Machine$double.eps^0.5, ...) {
    stopifnot(is.numeric(xa), length(xa) == 1,
              is.numeric(xb), length(xb) == 1)

    fun <- match.fun(f)
    f <- function(x) fun(x, ...)
    g <- function(x) (1/x^2) * f(1/x)

    # Quadrature function will be selectable in the future
    integ <- function(f, xa, xb)
        integrate(f, xa, xb, subdivisions = 512, rel.tol = tol)$value

    if (xa == xb) {
        Q <- 0

    } else if (xa > xb) {
        Q <- -1 * quadinf(f, xb, xa)

    } else if (is.finite(xa) && is.finite(xb)) {
        Q <- integ(f, xa, xb)

    } else if (xa == -Inf && xb == Inf) {
        Q <- integ(g, -1, 0) + integ(f, -1, 1) + integ(g, 0, 1)

    } else if (is.finite(xa) && xb == Inf) {
        if (xa > 0)
            Q <- integ(g, 0, 1/xa)
        else
            Q <- integ(f, xa, 1) + integ(g, 0, 1)

    } else if (xa == -Inf && is.finite(xb)) {
        if (xb < 0)
            Q <- integ(g, 1/xb, 0)
        else
            Q <- integ(g, -1, 0) + integ(f, -1, xb)
    }

    return(Q)
}
