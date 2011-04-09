interp1 <- function (x, y, xi = x,
             method = c("constant", "linear", "nearest", "spline", "cubic"))
{
    if (!is.vector(x, mode="numeric") || !is.vector(y, mode="numeric"))
        stop("Arguments 'x' and 'y' must be numeric vectors.")
    nx <- length(x)
    if (length(y) != nx)
        stop("Arguments 'x' and 'y' must be vectors of the same length.")
    if (nx <= 1)
        stop("Arguments 'x', 'y' must have at least a length >=2 .")

    if (min(xi) < min(x) || max(xi) > max(x))
        stop("Points 'xi' outside of range of argument 'x'.")

    e <- try(method <- match.arg(method), silent = TRUE)
    if (class(e) == "try-error") {
        warning("Unknown method: will use 'linear' interpolation.")
        method <- "linear"
    }

    if (is.unsorted(x)) {  # necessary for method 'nearest'
        warning("Points in argument in 'x' unsorted; will be sorted.")
        o <- order(x)
        x <- x[o]; y <- y[o]
    }

    if (any(duplicated(x)))
        warning("There are duplicated values in 'x'; mean will be tried.")

    if (method == "constant" || method == "linear") {
        yi <- approx(x, y, xi, method = method)$y
    } else if (method == "nearest") {
        n <- length(x)
        xx <- c(x[1], (x[2:n] + x[1:(n-1)])/2, x[n])
        yy <- c(y, y[n])
        yi <- approx(xx, yy, xi, method = "constant")$y
    } else if (method == "spline") {
        spfun <- splinefun(x, y, method = "fmm")
        yi <- spfun(xi)
    } else
        stop(paste("Method", method, "not yet implemented."))

    return(yi)
}
