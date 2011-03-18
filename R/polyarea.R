###
### POLYAREA.R  Determine area of a polygon
###

polyarea <- function(x, y) {
    if (length(x) == 0 && length(y) == 0) return(0)
    if (!(is.numeric(x) || is.complex(x)) ||
        !(is.numeric(y) || is.complex(y)))
        stop("Arguments 'x' and 'y' must be real or complex.")
    if (is.null(dim(x))) x <- matrix(x, length(x), 1)
    if (is.null(dim(y))) y <- matrix(y, length(y), 1)
    if (any(dim(x) != dim(y)))
        stop("Matrices 'x' and 'y' must be of same size.")

    n <- nrow(x); m <- ncol(x)
    z <- numeric(m)
    for (i in 1:m) {
        xi <- x[, i]
        yi <- y[, i]
        # Gauss' formula
        p1 <- sum(xi[1:(n-1)]*yi[2:n]) + xi[n]*yi[1]
        p2 <- sum(xi[2:n]*yi[1:(n-1)]) + xi[1]*yi[n]
        z[i] <- 0.5*(p1-p2)
    }
    return(abs(z))
}
