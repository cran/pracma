##
##  l a m b e r t W . R  Lambert W Function
##


lambertWp <- function(z) {
    if (!is.numeric(z))
        stop("Argument 'z' must be a numeric (real) vector.")

    if (length(z) == 1) {
        if (z  < -1/exp(1)) return(NA)
        if (z == -1/exp(1)) return(-1)
        w0 <- 1
        w1 <- w0 - (w0*exp(w0) - z)/((w0+1)*exp(w0) - (w0+2)*(w0*exp(w0) - z)/(2*w0+2))
        n <- 1
        while(abs(w1-w0) > 1e-15 && n <= 20) {
            w0 <- w1
            w1 <- w0 - (w0*exp(w0) - z)/((w0+1)*exp(w0) - (w0+2)*(w0*exp(w0) - z)/(2*w0+2))
            n <- n + 1
        }
        if (n > 20)
            warning("Maximum number of iterations has been exceeded.")
        return(w1)
    } else {
        sapply(z, lambertWp)
    }
}

D_lambertWp <- function(z) {
    if (!is.numeric(z))
        stop("Argument 'z' must be a numeric (real) vector.")

    if (length(z) == 1) {
        zw <- lambertWp(z)
        return( 1 / (1+zw) / exp(zw) )
    } else {
        sapply(z, D_lambertWp)
    }
}
