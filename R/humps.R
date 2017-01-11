##
##  h u m p s . R  Matlab Test Function
##


humps <- function(x) {
    if (missing(x))
        x <- seq(0.0, 1.0, by=0.05)

    stopifnot(is.numeric(x))

    1/((x-0.3)^2 + 0.01) + 1/((x-0.9)^2 + 0.04) - 6
}

sinc <- function(x) {
    stopifnot(is.numeric(x))

    sin(pi * x) / (pi * x)
}

psinc <- function(x, n) {
    stopifnot(is.numeric(x), is.numeric(n))
    if (floor(n) != ceiling(n) || n < 1)
        stop("Argument 'n' must be a positive integer.")
    
    sin(x * n/2) / n * sin(x/2)
}
