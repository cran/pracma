##
##  m o d u l a r . R  Modular functions
##

ceil <- function(n) ceiling(n)

fix <- function(n) trunc(n)

idiv <- function(n, m) return(n %/% m)

mod <- function(n, m) {
    stopifnot(is.numeric(n), is.numeric(m))
    if (length(m) != 1 || floor(m) != ceiling(m))
        stop("Argument 'm' must be an integer scalar.")
    if (m == 0) return(n)
    else        return(n %% m)
}

rem <- function(n, m) {
    stopifnot(is.numeric(n), is.numeric(m))
    if (length(m) != 1 || floor(m) != ceiling(m))
        stop("Argument 'm' must be an integer scalar.")
    if (m == 0) return(NaN)
    k <- mod(n, m)
    if (sign(n) * sign(m) < 0) {
        k <- k - m
    }
    return(k)
}

extgcd <- function(a, b) {
	# The Blankinship method, MAA Mathematical Monthly, Jul-Aug 1963
	stopifnot(is.numeric(a), length(a) == 1, floor(a) == ceiling(a), 
	          is.numeric(b), length(b) == 1, floor(b) == ceiling(b))

	sign_ab <- sign(c(a, b))
	A <- matrix(c(abs(c(a, b)), 1, 0, 0, 1), nrow=2, ncol=3)

	while (A[1, 1]*A[2, 1] != 0) {
		if (A[1, 1] > A[2, 1]) {
			m <- A[1, 1] %/% A[2, 1]
			A[1, ] <- A[1, ] - m * A[2, ]
		} else {
			m <- A[2, 1] %/% A[1, 1]
			A[2, ] <- A[2, ] - m * A[1, ]
		}
	}

	if (A[1, 1] == 0)  g <- A[2, ]
	else               g <- A[1, ]

	g[2:3] <- sign_ab * g[2:3]
	return(g)
}

gcd <- function(n, m) return(extgcd(n, m)[1])

lcm <- function(n, m) return(n / gcd(n, m) * m)

coprime <- function(n, m) {
	if (gcd(n, m) > 1) FALSE else TRUE
}

modinv <- function(n, m) {
	v <- extgcd(n, m)
	if (v[1] == 0 || v[1] > 1) return(NA)
	if (v[2] >= 0) v[2] else v[2] + m
}
