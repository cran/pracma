##
##  b i s e c t . R
##


bisect <- function(f, a, b, maxiter=100, tol=.Machine$double.eps^0.5)
# Bisection search for zero of a univariate function in a bounded interval
{
	if (f(a)*f(b) > 0) stop("f(a) and f(b) must have different signs.")
	x1 <- min(a, b); x2 <- max(a,b)
	xm <- (x1+x2)/2.0
	n <- 0
	while (abs(x1-x2)/2.0 > tol) {
		n <- n+1
		if (abs(f(xm)) <= tol) break
		if (f(x1)*f(xm) < 0) {
			x2 <- xm
		} else {
			x1 <- xm
		}
		xm <- (x1+x2)/2.0  # xm <- x1 - f(x1) * (x2-x1) / (f(x2)-f(x1))
		if (n >= maxiter) break
	}
	return(list(root=xm, f.root=f(xm), iter=n, estim.prec=abs(x1-x2)/2.0))
}


regulaFalsi <- function(f, a, b, maxiter=100, tol=.Machine$double.eps^0.5)
#Regula Falsi search for zero of a univariate function in a bounded interval
{
	x1 <- a; x2 <- b
	f1 <- f(x1); f2 <- f(x2)
	if (f1*f2 > 0) stop("f(a) and f(b) must have different signs.")

	m <- 0.5  # Illinois rule
	niter <- 0
	while (abs(x2-x1) >= tol && niter <= maxiter) {
		niter <- niter + 1
		z <- (x1*f2-x2*f1)/(f2-f1); fz <- f(z)

		if(fz*f2 < 0) {
			x1 <- x2; f1 <- f2
			x2 <- z;  f2 <- fz
		} else {
			# m <- f2/(f2+fz)                         # Pegasus rule
			# m <- if (1-fz/f2 > 0) 1-fz/f2 else 0.5  # Andersen/Bjoerk
			f1 <- m*f1
			x2 <- z;  f2 <- fz
		}
	}
	if (niter > maxiter && abs(x2-x1) >= tol)
		cat("regulaFalsi stopped without converging.\n")
	return(list(zero=z, fzero=fz, niter=niter, estim.prec=x1-x2))
}
