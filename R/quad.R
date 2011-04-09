##
##  q u a d . R  Adaptive Simpson Quadrature
##


quad <- function(f, xa, xb, tol = .Machine$double.eps^0.5, trace = FALSE, ...)
{
    stopifnot(is.numeric(xa), length(xa) == 1,
              is.numeric(xb), length(xb) == 1)
    fun <- match.fun(f)
    f <- function(x) fun(x, ...)

    Q <- .adaptsim(f, xa, xb, tol, trace)
    return(Q)
}


.adaptsim <- function(f, xa, xb, tol = tol, trace = trace)
{
    x <- c(xa, (xa+xb)/2, xb)
    y <- c(f(xa), f((xa+xb)/2), f(xb))  # f(x)
    fa <- y[1]; fm <- y[2]; fb <- y[3]
    yy <- f(xa + c(0.9501, 0.2311, 0.6068, 0.4860, 0.8913) * (xb-xa))
    is <- (xb - xa)/8 * (sum(y)+sum(yy))
    if (is == 0) is <- xb-xa
    is <- is * tol/.Machine$double.eps

    Q <- .adaptsimstp(f, xa, xb, fa, fm, fb, is, trace)
	return(Q)
}

.adaptsimstp <- function(f, xa, xb, fa, fm, fb, is, trace)
{
    m <- (xa + xb)/2; h <- (xb - xa)/4
    x <- c(xa + h, xb - h)
    y <- c(f(xa + h), f(xb - h))  # f(x)
    fml <- y[1]; fmr <- y[2]
    i1 <- h/1.5 * (fa + 4*fm + fb)
    i2 <- h/3 * (fa + 4*(fml + fmr) + 2*fm + fb)
    i1 <- (16*i2 - i1)/15
    if ( (is + (i1-i2) == is) || (m <= xa) || (xb<=m) ) {
        if ( ((m <= xa) || (xb<=m))) {
            warning("Required tolerance may not be met.\n")
        }
        Q <- i1
        if (trace) cat(xa, xb-xa, Q, "\n")
    } else {
        Q <- .adaptsimstp (f, xa, m, fa, fml, fm, is, trace) +
             .adaptsimstp (f, m, xb, fm, fmr, fb, is, trace)
    }
    return(Q)
}
