##
##  q u a d i n f . R  Infinite Integrals
##


quadinf <- function(f, xa, xb, tol = .Machine$double.eps^0.5,
                        method = NULL, ...) {
    stopifnot(is.numeric(xa), length(xa) == 1,
              is.numeric(xb), length(xb) == 1)

    fun <- match.fun(f)
    f <- function(x) fun(x, ...)
    g <- function(x) (1/x^2) * f(1/x)

    if (is.null(method)) {
        integ <- function(f, xa, xb)
            integrate(f, xa, xb, subdivisions = 512, rel.tol = tol)$value

    } else {
        methods <- c("Kronrod","Richardson","Clenshaw","Simpson","Romberg")
        method  <- match.arg(method, methods)
        integ <- switch(method,
            "Kronrod"    = function(fct, xmin, xmax) quadgk(fct, xmin, xmax, tol = tol),
            "Richardson" = function(fct, xmin, xmax) quadgr(fct, xmin, xmax, tol = tol)$value,
            "Clenshaw"   = function(fct, xmin, xmax) quadcc(fct, xmin, xmax, tol = tol),
            "Romberg"    = function(fct, xmin, xmax) romberg(fct, xmin, xmax, tol = tol)$value,
            "Simpson"    = function(fct, xmin, xmax) simpadpt(fct, xmin, xmax, tol = tol)
            )
    }

    if (xa == xb) {
        Q <- 0

    } else if (xa > xb) {
        Q <- -1 * quadinf(f, xb, xa, tol = tol, method = method)

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
