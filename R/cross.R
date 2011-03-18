###
### CROSS.R  Vector product
###

cross <- function(x, y) {
	if (!is.numeric(x) || !is.numeric(y))
		stop("Arguments 'x' and 'y' must be numeric vectors or matrices.")
	crossp <- function(a, b) {
		axb <- c(a[2]*b[3] - a[3]*b[2],
		         a[3]*b[1] - a[1]*b[3],
		         a[1]*b[2] - a[2]*b[1])
		return(axb)
	}
	if (is.matrix(x) && min(dim(x) == 1)) x <- drop(x)
	if (is.matrix(y) && min(dim(y) == 1)) y <- drop(y)
	if (is.vector(x) && is.vector(y)) {
		if (length(x) == length(y) && length(x) == 3) {
			xxy <- crossp(x, y)
		} else {
			stop("Vectors 'x' and 'y' must be both of length 3.")
		}
	} else {
		if (is.matrix(x) && is.matrix(y)) {
			if (all(dim(x) == dim(y))) {
				if (ncol(x) == 3) {
					m <- nrow(x)
					xxy <- matrix(0, m, 3)
					for (i in 1:m)
						xxy[i,] <- crossp(x[i,], y[i,])
				} else {
					if (nrow(x) == 3) {
						m <- ncol(x)
						xxy <- matrix(0, 3, m)
						for (i in 1:m)
							xxy[,i] <- crossp(x[,i], y[,i])
					} else {
						stop("'x', 'y' must have one dimension of length 3.")
					}
				}
			} else {
				stop("Matrices 'x' and 'y' must be of same size.")
			}
		} else {
			if (is.vector(x) && is.matrix(y) ||
				is.matrix(x) && is.vector(y)) {
			stop("Arguments 'x', 'y' must be vectors/matrices of same size.")
				}
		}
	}
	return(xxy)
}
