##
##  n e w t o n s y s . R
##


# Compute the Jacobian as  J_{ij} = df_i/dx_j  for a vector-valued function
# w/o assuming that the f_i are vectorized.
jacobian <- function(f, x, h.eps=.Machine$double.eps^(1/3), ...) {
    if (!is.numeric(x) || length(x) == 0)
        stop("Argument 'x' must be a non-empty numeric vector.")

	n <- length(x)
	m <- length(f(x, ...))
	jacob <- matrix(NA, m, n)
	h <- numeric(n)
	for (i in 1:n) {
		h[i] <- h.eps
		jacob[, i] <- (f(x+h, ...) - f(x-h, ...))/(2*h.eps)
		# for (j in 1:m) {  # *this* should be vectorized
		# 	# jacob[j, i] <- (f(x+h, ...)[j] - f(x-h, ...)[j])/(2*h.eps)
		# }
		h[i] <- 0
	}
	return(jacob)
}


# Finds a zero of a nonlinear system by Newton's method
newtonsys <- function(Ffun, x0, Jfun = NULL, ...,
	                  maxiter = 100, tol = .Machine$double.eps^(1/2)) {
	vnorm <- function(x) { sqrt(sum(x^2)) }
	if (is.null(Jfun)) Jfun <- function(x, ...) jacobian(Ffun, x, ...)

	niter <- 0; err <- tol + 1
	x <- x0
	while (err >= tol && niter < maxiter) {
		niter <- niter + 1
		F <- Ffun(x, ...)
		J <- Jfun(x, ...)
		delta <- -1 * solve(J, F)
		x <- x + delta
		err <- vnorm(delta)
	}
	F <- vnorm(Ffun(x, ...))
	if (niter > maxiter && err > tol) {
		cat("Fails to converge within maximum number of iterations.\n",
		    "The iterate returned has relative residual ", F, "\n", sep="")
	}
	return(list(zero=x, fnorm=F, niter=niter))
}
