###
### POLYFIT.R  Polynom
###

polyfit <- function(x, y, n=1) {
    if (!is.numeric(x) || !is.numeric(y))
        stop("Arguments x and y must be numeric.")
    if (length(x) != length(y))
        stop("Vectors/matrices x and y must be of same length.")
    if (is.null(n) || n < 0 || ceiling(n) != floor(n))
        stop("Degree n must be a non-negative integer.")

    x <- x[1:length(x)]; y <- y[1:length(y)]
    A <- outer(x, seq(n, 0), "^")
    p <- qr.solve(A, y)
    return(p)
}
