##
##  n u m b e r s . R  Functions from Number Theory
##


.isnatural <- function(n) {
    if (is.numeric(n) && length(n) == 1 && 
        floor(n) == ceiling(n) && n >= 1) {
            TRUE
    } else {
        FALSE
    }
}


eulersPhi <- function(n) {
    if (!.isnatural(n))
        stop("Arguments 'n' must be a single positive integers.")

	m <- n
	for (p in unique(ifactor(n)))
		m <- m * (1 - 1/p)
	return(round(m))
}


moebiusFun <- function(n) {
    if (!.isnatural(n))
        stop("Arguments 'n' must be a single positive integers.")

	R <- rle(ifactor(n))
	if (n == 1) {
		r <- 1
	} else if (max(R$lengths) > 1) {
		r <- 0
	} else {
		r <- (-1)^length(R$values)
	}
	return(r)
}


mertensFun <- function(n) {
    if (!.isnatural(n))
        stop("Arguments 'n' must be a single positive integers.")

    sum(sapply(1:n, moebiusFun))
}


sigma <- function(n, k = 1, proper = FALSE) {
    if (!.isnatural(n))
        stop("Arguments 'n' must be a single positive integers.")
    if (!is.numeric(k) || length(k) != 1)
        stop("Argument 'k' must be a numeric scalar.")

	if (n == 1) return(if (proper) 0 else 1)
	R <- rle(ifactor(n))
	P <- 1
	for (i in 1:length(R$values)) {
		ri <- R$values[i]
		ai <- R$lengths[i]
		P <- P * sum(rep(ri, ai+1)^seq(0, ai*k, length=ai+1))
	}
	if (proper) P <- P - n^k
	return(P)
}


tau <- function(n) {  # Ramanujan's tau function
    if (!.isnatural(n))
        stop("Arguments 'n' must be a single positive integers.")

    if (n == 0) s <- 0
    else if (n == 1) s <- 1
    else {
        s <- 0
        for (k in 1:(n-1)) {
            s <- s + sigma(k, 5)*sigma(n-k, 5)
        }
        s <- 65/756 * sigma(n, 11) + 691/756 * sigma(n, 5) - 691/3 * s
    }
    return(s)
}


omega <- function(n) {
    if (!.isnatural(n))
        stop("Arguments 'n' must be a single positive integers.")

	if (n == 1) 0
	else length(unique(ifactor(n)))
}


Omega <- function(n) {
    if (!.isnatural(n))
        stop("Arguments 'n' must be a single positive integers.")

	if (n == 1) 0
	else sum(rle(ifactor(n))$length)
}
