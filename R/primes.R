###
### PRIMES.R  Prime numbers
###


primes <- function(n) {
    if (!is.numeric(n) || length(n) != 1)
        stop("Argument 'n' must be a numeric scalar.")
    n <- floor(n)
    if (n < 2) return(c())
    p <- seq(1, n, by=2)
    q <- length(p)
    p[1] <- 2
    if (n >= 9) {
        for (k in seq(3, sqrt(n), by=2)) {
            if (p[(k+1)/2] != 0)
                p[seq((k*k+1)/2, q, by=k)] <- 0
        }    
    }
    p[p > 0]
}


primes2 <- function(n1 = 1, n2 = 1000) {
    if (!is.numeric(n1) || length(n1) != 1 || floor(n1) != ceiling(n1) || n1 <= 0 ||
        !is.numeric(n2) || length(n2) != 1 || floor(n2) != ceiling(n2) || n2 <= 0 )
        stop("Arguments 'n1' and 'n2' must be integers.")

    if (n2 > 2^53 - 1)  stop("Upper bound 'n2' must be smaller than 2^53-1.")
    if (n1 > n2)        stop("Upper bound must be greater than lower bound.")
   
	if (n2 <= 1000) {
		P <- primes(n2)
		return(P[P >= n1])
	}

    Primes <- primes(sqrt(n2))

    N <- seq.int(n1, n2)
    n <- length(N)
    A <- numeric(n)
    if (n1 == 1) A[1] <- -1

    for (p in Primes) {
        a <- numeric(n)
        r <- n1 %% p                                    # rest modulo p
        if (r == 0) { i <- 1 } else { i <- p - r + 1 }  # find next divisible by p
        if (i <= n && N[i] == p) { i <- i + p }         # if it is p itself, skip
        while (i <= n) { a[i] <- 1; i <- i + p }        # mark those divisible by p
        A <- A + a
    }
    return(N[A == 0])
}


twinPrimes <- function(n1, n2) {
	P <- primes2(n1, n2)
	twins <- which(diff(P) == 2)
	cbind(P[twins], P[twins+1])
}


nextPrime <-function(n) {
	if (n <= 1)  n <- 1  else  n <- floor(n)
	n <- n + 1

	# m <- 2*n  # Bertrands law
	d1 <- max(3, round(log(n)))
	P  <- primes2(n, n + d1)

	while(length(P) == 0) {
		n <- n + d1 + 1
		P  <- primes2(n, n + d1)
	}
	return( min(P) )
}
