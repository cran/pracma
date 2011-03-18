##
##  h o r n e r . R  Horner Scheme
##


horner <- function(p, x) {
# Horner's rule to compute p(x) and p'(x) vectorized for the
# polynomial  p = p_1*x^n + p_2*x^{n-1} + ... + p_n*x + p_{n+1}
	n <- length(p); m <- length(x)
	if (n == 0) { y <- dy <- rep(NA, m) }
	else if (n == 1) { y <- rep(p, m); dy <- rep(0, m) }
	else {
		y <- p[1]; dy <- 0
		for (i in 2:n) {
			dy <- dy * x + y
			y  <-  y * x + p[i]
		}
	}
	return(list(y = y, dy = dy))
}
