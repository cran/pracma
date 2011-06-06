##
##  c o n v . R  Polynomial Convolution
##


conv <- function(x, y) {
    if ( (!is.vector(x, mode="numeric") && !is.vector(x, mode="complex")) ||
         (!is.vector(y, mode="numeric") && !is.vector(y, mode="complex")) )
        stop("Arguments 'x' and 'y' must be real or complex vectors.")

    lx <- length(x)
    ly <- length(y)
    n <-  lx +  ly - 1
    z <- fft(fft(c(x, rep(0, n - lx))) * fft(c(y, rep(0, n - ly))),
             inverse = TRUE) / n

    if (is.numeric(x) && is.numeric(y))
        z <- Re(z)
    return(z)
}

ifft <- function(x) {
    if (length(x) == 0)
        return(c())
    if ( (!is.vector(x, mode="numeric") && !is.vector(x, mode="complex")))
        stop("Argument 'x' must be real or complex vector.")
    
    fft(x, inverse = TRUE) / length(x)
}
