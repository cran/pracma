hooke_jeeves <- function(x0, f, h = 1, scale = 1,
                         info = FALSE, tol = .Machine$double.eps^(2/3), ...) {

    if (!is.numeric(x0))
        stop("Argument 'x0' must be a numeric vector.", call. = FALSE)
    x0 <- c(x0)
    if (!is.numeric(scale) || length(scale) != 1)
        stop("Argument 'scale' must be a numeric scalar.", call. = FALSE)
    if (!is.numeric(h) || length(h) != 1 || h <= 0)
        stop("Argument 'h' must be a positive scalar.", call. = FALSE)

    fun <- match.fun(f)
    f <- function(x) scale * fun(x, ...)

    # Count number of evaluations
    hjeval <- local({
        m <- 0
        function(f, x) {
            if (missing(f)) return(m)
            m <<- m + 1
            return(f(x))
        }
    })

    # Exploration phase
    hjexplore <- function(x0, f, n, h) {
        x <- x0; fx <- hjeval(f, x)
        for (i in sample.int(n, n)) {
            x[i] <- x[i] + h
            ff <- hjeval(f, x)
            if (ff < fx) {
                fx <- ff
            } else {
                x[i] <- x0[i] - h
                ff <- hjeval(f, x)
                if (ff < fx) {
                    fx <- ff
                } else {
                    x[i] <- x0[i]
                }
            }
        }
        return(list(x = x, f = fx))  # shall improvement be returned?
    }

    # Main loop
    n <- length(x0)
    x <- x0;
    minf <- hjeval(f, x)

    # Move phase
    iter <- 0;
    while (h > tol) {
        y <- hjexplore(x, f, n, h)$x
        if (all(y == x)) {
            h = h/2
        } else {
            x <- 2 * y - x
            z <- hjexplore(x, f, n, h)$x
            if (all(z == x)) {
                x <- y
            } else {
                x <- z
            }
        }
        iter <- iter + 1
    }

    # End program
    fc <- hjeval(f, x)
    nc <- hjeval()
    return(list(xmin = x, fmin = fc, fcalls = nc, niter = iter))
}
