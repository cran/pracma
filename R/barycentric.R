##
##  b a r y l a g . R  Barycentric Lagrange Interpolation
##


barycentricInterp <- function(xi, yi, x) {
    stopifnot(is.vector(xi, mode="numeric"), is.vector(xi, mode="numeric"))
    if (!is.numeric(x))
        stop("Argument 'x' must be a numeric vector or matrix.")

	n <- length(xi); m <- length(x)

	# Check the input arguments
	if (length(yi) != n)
		stop("Node vectors xi an yi must be of same length.")
	if ( min(x) < min(xi) || max(x) > max(xi) )
		stop("Some interpolation points outside the nodes.")

	# Compute weights
	X  <- matrix(rep(xi, times=n), n, n)
	wi <- 1 / apply(X - t(X) + diag(1, n, n), 1, prod)

	# Distances between nodes and interpolation points
	Y <- outer(x, xi, "-")

	# Identify interpolation points that are nodes
	inds <- which(Y == 0, arr.ind=TRUE)
	Y[inds] <- NA

	# Compute the values of interpolated points
	W <- matrix(rep(wi, each=m), m, n) / Y
	y <- (W %*% yi) / apply(W, 1, sum)

	# Replace with values at corresponding nodes
	y[inds[,1]] <- yi[inds[,2]]

	# Return interpolation values as vector
	return(y[,])
}
