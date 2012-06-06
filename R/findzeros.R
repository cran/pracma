findzeros <- function(f, a, b, n = 100, tol = .Machine$double.eps^(2/3), ...) {
    stopifnot(is.numeric(a), length(a) == 1,
              is.numeric(b), length(b) == 1,
              is.numeric(n), floor(n) == ceiling(n), n >= 2)
    if (! a < b)
        stop("Left interval border must be smaller than right one.")


    fun <- match.fun(f)
    f <- function(x) fun(x, ...)

	h <- (b - a) / n
	x <- seq(a, b, by = h)  # length(x) == n+1

    if (abs(f(x[1])) < tol/10) R <- c(x[1])
    else              R <- c()

	for (i in 2:n) {
        if (f(x[i-1]) * f(x[i]) < 0) {
		    u <- uniroot(f, c(x[i-1], x[i]))
		    R <- c(R, u$root)
		}
		s <- (f(x[i]) - f(x[i-1])) * (f(x[i+1]) - f(x[i]))
		if (abs(f(x[i])) < tol && s < 0) {
			R <- c(R, x[i])
		}
	}

    if (abs(f(x[n+1])) < tol/10) R <- c(R, x[n+1])

	return(R)
}


findmins <- function(f, a, b, n = 100, tol = .Machine$double.eps^(2/3), ...) {
    stopifnot(is.numeric(a), length(a) == 1,
              is.numeric(b), length(b) == 1,
              is.numeric(n), floor(n) == ceiling(n), n >= 2)
    if (! a < b)
        stop("Left interval border must be smaller than right one.")

    fun <- match.fun(f)
    f <- function(x) fun(x, ...)

	h <- (b - a) / n
	x <- seq(a, b, by = h)  # length(x) == n+1

    R <- c()

    for (i in 2:(n-1)) {
        if ( (f(x[i]) - f(x[i-1]) < 0) && (f(x[i+1]) - f(x[i])) > 0 ) {
            o <- optimize(f, c(x[i-1], x[i+1]))
            R <- c(R, o$minimum)
        }
    }

    return(R)
}
