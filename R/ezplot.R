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
