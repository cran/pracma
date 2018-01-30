##
##  b s x f u n . R
##


bsxfun <- function(func, x, y) {
    stopifnot(is.numeric(x), is.numeric(y))
    # fun <- match.fun(f)
    # f <- function(x, y) fun(x, y, ...)

    dx <- dim(x); dy <- dim(y)
    if ( is.vector(x) && is.vector(y) ) {
        z <- mapply(func, x, y)

    } else if (is.matrix(x) && is.matrix(y) && all(dx == dy)) {
        z <- sweep(x, y, MARGIN = c(1, 2), FUN = func)
        # dim(z) <- dx
        
    } else if (is.array(x)  && is.array(y) && all(dx == dy)) {
        z <- mapply(func, x, y)
        dim(z) <- dx

    } else {
        stop("Argument 'x', 'y' must be vectors or arrays of the same size.")
    }
    return(z)
}


arrayfun <- function(func, ...) {
    # func <- match.fun(func)

    dots <- list(...)
    if (length(dots) < 1)
        stop("Empty list of arrays: Result cannot be computed.")
    
    d <- dim(dots[[1]])        # no test on array sizes to be fast
    r <- mapply(func, ...)     # no try ... catch, number of variables
    dim(r) <- d
    return(r)
}
