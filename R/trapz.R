###
### TRAPZ.R  Numerical integration by trapezoidal rule
###

trapz <- function(x, y) {
    if (missing(y)) {
        if (length(x) == 0) return(0)
        y <- x
        x <- 1:length(x)
    }
    if (length(x) == 0) return(0)
    if (!(is.numeric(x) || is.complex(x)) ||
        !(is.numeric(y) || is.complex(y)))
        stop("Arguments 'x' and 'y' must be real or complex.")

    m <- length(x)
    xp <- c(x, x[m:1])
    yp <- c(numeric(m), y[m:1])
    n <- 2*m
    p1 <- sum(xp[1:(n-1)]*yp[2:n]) + xp[n]*yp[1]
    p2 <- sum(xp[2:n]*yp[1:(n-1)]) + xp[1]*yp[n]
    return(0.5*(p1-p2))
}
