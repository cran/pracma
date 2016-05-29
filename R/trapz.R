##
##  t r a p z . R  Numerical integration by trapezoidal rule
##


trapz <- function(x, y) {
    if (missing(y)) {
        if (length(x) == 0) return(0)
        y <- x
        x <- seq(along=x)
    }
    if (length(x) == 0 && length(y) == 0) return(0)
    if (!(is.numeric(x) || is.complex(x)) ||
            !(is.numeric(y) || is.complex(y)) )
        stop("Arguments 'x' and 'y' must be real or complex vectors.")
    m <- length(x)
    if (length(y) != m)
        stop("Arguments 'x', 'y' must be vectors of the same length.")
    if (m <= 1) return(0.0)

    # z <- sum((x[2:m] - x[1:(m-1)]) * (y[1:(m-1)] + y[2:m]))
    # return(0.5 * z)

    xp <- c(x, x[m:1])
    yp <- c(numeric(m), y[m:1])
    n <- 2*m
    p1 <- sum(xp[1:(n-1)]*yp[2:n]) + xp[n]*yp[1]
    p2 <- sum(xp[2:n]*yp[1:(n-1)]) + xp[1]*yp[n]

    return(0.5*(p1-p2))
}


cumtrapz <- function(x, y) {
    if (missing(y)) {
        if (length(x) == 0) return(0)
        y <- x
        x <- 1:length(x)
    }
    if (length(x) == 0) return(0)
    if (!(is.numeric(x) || is.complex(x)) ||
        !(is.numeric(y) || is.complex(y)))
        stop("Arguments 'x' and 'y' must be real or complex.")

    x <- as.matrix(c(x))
    m <- length(x)
    if (is.vector(y)) y <- as.matrix(y)
    if (nrow(y) != m)
        stop("Arguments 'x' and 'y' are not compatible: nrow(y) != length(x).")

    n  <- ncol(y)
    dt <- repmat(diff(x)/2, 1, n)
    ct <- apply(dt * (y[1:(m-1), ] + y[2:m, ]), 2, cumsum)

    return(rbind(zeros(1, n), ct))
}


trapzfun <- function(f, a, b, maxit = 25, tol = 1e-07, ...) {
    stopifnot(is.numeric(a), length(a) == 1, is.finite(a),
              is.numeric(b), length(b) == 1, is.finite(b))

    fun <- match.fun(f)
    f <- function(x) fun(x, ...)

    if (a == b) return(list(area = 0.0, iter = 0, error = 0))
	
    n <- 1
    h <- b - a
    T <- h * (f(a) + f(b)) / 2.0

    for (i in 1:maxit) {
        M <- 0
        for (j in 0:(n-1)) {
            M <- M + f(a + (j + 0.5) * h)
        }
        M <- h * M
        T <- (T + M) / 2.0
        h <- h / 2.0
        n <- 2 * n
        err <- abs(T - M)
        if (err < tol) break
    }

    return(list(value = T, iter = i, rel.err = err))
}
