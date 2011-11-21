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
	x1 <- a;      x2 <- b
	f1 <- f(x1);  f2 <- f(x2)
	if (f1*f2 > 0) stop("f(a) and f(b) must have different signs.")

	m <- 0.5                                         # Illinois rule
	niter <- 0
	while (abs(x2-x1) >= tol && niter <= maxiter) {
		niter <- niter + 1
		x3 <- (x1*f2-x2*f1)/(f2-f1); f3 <- f(x3)

		if(f3*f2 < 0) {
			x1 <- x2;  f1 <- f2
			x2 <- x3;  f2 <- f3
		} else {
			# m <- f2/(f2+f3)                         # Pegasus rule
			# m <- if (1-f3/f2 > 0) 1-f3/f2 else 0.5  # Andersen/Bjoerk
			f1 <- m * f1
			x2 <- x3;  f2 <- f3
		}
	}
	if (niter > maxiter && abs(x2-x1) >= tol)
		cat("regulaFalsi stopped without converging.\n")
	return(list(root = x3, f.root = f3, niter = niter, estim.prec = x1-x2))
}


ridder <- function(f, a, b, maxiter = 100, tol = .Machine$double.eps^0.5) {
	x1 <- a;      x2 <- b
	f1 <- f(x1);  f2 <- f(x2)
	if (f1*f2 >= 0) stop("f(a) and f(b) must have different signs.")

    niter <- 2
    while(abs(x1 - x2) > tol && niter < maxiter) {
        xm <- (x1 + x2)/2;  fm <- f(xm)
        if (fm == 0)
            return(list(root = xm, f.root = 0, niter = niter, estim.prec = 0))

        x3 <- xm + (xm - x1) * sign(f1 - f2) * fm / sqrt(fm^2 - f1 * f2)
        f3 <- f(x3);  niter <- niter + 2
        if (f3 == 0)
            return(list(root = x3, f.root = 0, niter = niter, estim.prec = 0))

        if (fm * f3 < 0) {
            x1 <- xm;  f1 <- fm
            x2 <- x3;  f2 <- f3
        } else if (f1 * f3 < 0) {
            x2 <- xm;  f2 <- fm
        } else if (f2 * f3 < 0) {
            x1 <- x3;  f1 <- f3
        } else {
            stop("Inform the maintainer: you should never get here.")
        }
     }

    if (abs(f1) < abs(f2)) {
        x0 <- x1;  f0 <- f1
    } else {
        x0 <- x2;  f0 <- f2
    }
    ep <- abs(x1 - x2)
    return(list(root = x0, f.root = f0, niter = niter, estim.prec = ep))
}
