##
##  p o l a r . R  Random points in unit circle
##


polar <- function(n = 1, r = 1) {
	d <- 2
	while (d > 1) {
		u <- 2 * runif(2) - 1
		d <- t(u) %*% u
	}

	if (n == 1) {
		U <- u
	} else {
		U <- matrix(NA, nrow = n, ncol = 2)
		U[1, ] <- u
		for (i in 2:n) {
			d <- 2
			while (d > 1) {
				u <- 2 * runif(2) - 1
				d <- t(u) %*% u
			}
			U[i, ] <- u
		}
	}
	return(r * U)
}
