##
##  a c c u m a r r a y . R  Accumulate Vector Elements
##


uniq <- function(a, first = FALSE) {
    if (length(a) == 0)
        return(list(b = c(), m = c(), n = c()))
    if (!is.numeric(a) || !is.vector(a))
        stop("Argument 'a' must be a numeric vector.")

    la <- length(a); n <- numeric(la)
    u  <- unique(a)
    lu <- length(u); m <- numeric(lu)

    mima <- if (first) min else max

    for (i in 1:lu) {
        w <- which(a == u[i])
        m[i] <- mima(w)
        n[w] <- i 
    }

    return(list(b = u, m = m, n = n))
}


accumarray <- function(is, a, func = sum) {
    stopifnot(is.numeric(is), is.numeric(a))
    if (length(is) != length(a))
        stop("Arrays 'subs' and 'is' must be of the same length.")

	n <- max(is)
	A <- numeric(n)

	for (i in unique(is)) {
		js <- which(is == i)
		A[i] <- func(a[js])
	}

	return(A)
}
