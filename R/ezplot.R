##
##  e z p l o t . R
##


ezplot <- function(f, a, b, n = 101, col = "blue",
                   grid = TRUE, gridcol = "gray", 
                   fill = FALSE, fillcol = "lightgray",
                   xlab = "x", ylab = "f (x)", main = "Function Plot", ...) {
    fun <- match.fun(f)
    f <- function(x) fun(x)
    stopifnot(is.numeric(a), is.numeric(b),
              length(a) == 1, length(b) == 1, a < b)

    x <- seq(a, b, length.out = n)
    y <- f(x)
    plot(x, y, type = "n", xlab = xlab, ylab = ylab, main = main, ...)

    if (grid)
        grid(col = gridcol)

    if (fill) {
        xx <- c(x, rev(x))
        yy <- c(rep(0, length(x)), rev(y))
        polygon(xx, yy, col = fillcol, border = "darkgray")
    }

    lines(x, y, col = col)
    invisible(NULL)
}


ezcontour <- function(f, xlim = c(-pi,pi), ylim = c(-pi,pi), 
                         n = 60, filled = FALSE, col = NULL) {
    fun <- match.fun(f)
    f <- function(x) fun(x)
    stopifnot(is.numeric(xlim), is.numeric(ylim),
              length(xlim) == 2, length(ylim) == 2,
              xlim[1] < xlim[2], ylim[1] < ylim[2])

    xx <- linspace(xlim[1], xlim[2], n)
    yy <- linspace(ylim[1], ylim[2], n)
    F <- matrix(NA, n, n)
    for (i in 1:n) {
        for (j in 1:n) {
            F[i, j] <- f(c(xx[i], yy[j]))
        }
    }
    if (filled) {
        if (is.null(col)) col <- heat.colors(12)
        image(xx, yy, F, col = col)
        contour(xx, yy, F, add = TRUE)
    } else {
        if (is.null(col)) col <- "black"
        contour(xx, yy, F)
        grid()
    }
    invisible(NULL) 
}


ezmesh <- function(f, xlim = c(-pi,pi), ylim = c(-pi,pi), 
                         n = 60, col = "lightgray", ...) {
    fun <- match.fun(f)
    f <- function(x) fun(x)
    stopifnot(is.numeric(xlim), is.numeric(ylim),
              length(xlim) == 2, length(ylim) == 2,
              xlim[1] < xlim[2], ylim[1] < ylim[2])

    xx <- linspace(xlim[1], xlim[2], n)
    yy <- linspace(ylim[1], ylim[2], n)
    F <- matrix(NA, n, n)
    for (i in 1:n) {
        for (j in 1:n) {
            F[i, j] <- f(c(xx[i], yy[j]))
        }
    }
    persp(xx, yy, F, col = col, ...)
    invisible(NULL)
}
