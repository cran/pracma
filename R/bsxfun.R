##
##  b s x f u n . R
##


bsxfun <- function(f, x, y, ...) {
    stopifnot(is.numeric(x), is.numeric(y))

    fun <- match.fun(f)
    f <- function(x, y) fun(x, y, ...)

    dx <- dim(x); dy <- dim(y)
    if ( is.vector(x) && is.vector(y) ) {
        z <- mapply(f, x, y)

    } else if (is.array(x)  && is.array(y) && dx == dy) {
        z <- mapply(f, x, y)
        dim(z) <- dx

    } else {
        stop("Argument 'x', 'y' must be vectors or arrays of the same size.")
    }
    return(z)
}
