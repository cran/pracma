###
### p o l y a r e a . R  Calculate area and center of a polygon
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
    return(z)
}


poly_center <- function(x, y) {
    stopifnot(is.numeric(x), is.numeric(y))
    n <- length(x)
    if (length(y) != n || n <= 2)
        stop("Arguments 'x' and 'y' must be of the same length >= 3.")

    parea <- polyarea(x, y)
    if (parea == 0)
        return(c(NA, NA))

    x1 <- x[1:(n-1)]; x2 <- x[2:n]
    y1 <- y[1:(n-1)]; y2 <- y[2:n]
    xy <- x1*y2 - x2*y1

    cx <- sum((x1+x2) * xy)
    cy <- sum((y1+y2) * xy)
    return(1/parea/6 * c(cx, cy))
}


poly_length <- function(x, y) {
    stopifnot(is.numeric(x), is.numeric(y))

    X  <- cbind(x, y)
    dX <- diff(X)
    return(sum(sqrt(rowSums(dX^2))))
}
