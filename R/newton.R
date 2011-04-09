##
##  n e w t o n . R  Newton Root finding
##


newton <- function(fun, x0, dfun = NULL, ...,
	               maxiter = 100, tol = .Machine$double.eps^0.5) {
	# Newton method for finding function zeros
	if  (is.null(dfun)) {
		dfun <- function(x, ...) { h <- tol^(2/3)
			(fun(x+h, ...) - fun(x-h, ...)) / (2*h)
		}
	}
	x   <- x0
	fx  <- fun(x, ...)
	dfx <- dfun(x, ...)
	niter <- 0
	diff  <- tol + 1
	while (diff >= tol && niter <= maxiter) {
		niter <- niter + 1
		diff  <- - fx/dfx
		x <- x + diff
		diff <- abs(diff)
		fx  <- fun(x, ...)
		dfx <- dfun(x, ...)
	}
	if (niter > maxiter) {
		warning("Maximum number of iterations 'maxiter' was reached.")
	}
	return(list(zero=x, fzero=fx, niter=niter, estim.prec=diff))
}


secant <- function(fun, a, b, ...,
                   maxiter = 100, tol = .Machine$double.eps^0.5)
# Secant search for zero of a univariate function
{
    fun <- match.fun(fun)
    f <- function(x) fun(x, ...)

	x1 <- a; x2 <- b
	f1 <- f(x1); if (abs(f1) <= tol) return(x1)
	f2 <- f(x2); if (abs(f2) <= tol) return(x1)
	n <- 0
	while (n <= maxiter && abs(x2 - x1) > tol) {
		n <- n+1
		slope <- (f2 - f1)/(x2 - x1)
		if (slope == 0) return(root=NA, f.root=NA, iter=n, estim.prec=NA)
		x3 <- x2 - f2/slope
		f3 <- f(x3); if (abs(f3) <= tol) break
		x1 <- x2; f1 <- f2
		x2 <- x3; f2 <- f3
	}
	if (n > maxiter) {
		warning("Maximum number of iterations 'maxiter' was reached.")
	}
	return(list(root=x3, f.root=f3, iter=n, estim.prec=2*abs(x3-x2)))
}
