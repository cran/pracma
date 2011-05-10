##
##  f a c t . R  Factorial Function
##


fact <- function(n) {
    if (!is.numeric(n))
        stop("Argument 'n' must be a numeric vector or matrix.")

    d <- dim(n)
    n <- c(n)
    n <- floor(n)

    f <- rep(1, length(n))
    i0 <- which(n > 170)
    if (length(i0) > 0) f[i0] <- Inf
    i1 <- which(n < 0)
    if (length(i1) > 0) f[i1] <- NaN
    i2 <- which(is.na(n))
    if (length(i2) > 0) f[i2] <- NA

    ii <- which(n > 0 & n <= 170)
    if (length(ii) > 0) {
        nn <- n[ii]
        ff <- numeric(length(nn))

        s = 1
        for (k in 1:max(nn)) {
            s <- s * k
            p <- which(nn == k)
            ff[p] <- s
        }
        f[ii] <- ff
    }

    dim(f) <- d
    return(f)
}
