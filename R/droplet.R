##
##  d r o p l e t . R  Droplet Functions for e and pi
##


droplet_e <- function(n) {
	stopifnot(is.numeric(n), length(n) == 1, n >= 1)
	if (n > 1000)
		cat("Warning: Calculating n > 1000 digits will take a long time!\n")

	n <- n + 1
    a <- numeric(n+1) + 1.0

    E <- "2."
    for (i in 1:(n-1)) {
    	a <- 10 * a
    	for (j in (n+1):2) {
    		a[j-1] <- a[j-1] + a[j] %/% (j+1)
    		a[j]   <- a[j] %%  (j+1)
    	}
    	E <- paste(E, a[1] %/% 2, sep="")
    	a[1] <- a[1] %% 2
    }
    return(E)
}



