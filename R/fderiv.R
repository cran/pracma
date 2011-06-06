##
##  f d e r i v . R  Numerical Differentiation
##

fderiv <- function(f, x, n = 1, h = 0,
                   method = "central", ...) {
    if (length(x) == 0) return(c())
    if (!is.numeric(x))
        stop("Argument 'x' must be a number or a numeric vector.")
    if (! n %in% c(0, 1, 2, 3, 4))
        stop("The order of the derivative, 'n', can only be 0, 1, 2, 3, or 4.")

    fun <- match.fun(f)
    f <- function(x) fun(x, ...)

    fx <- f(x)
    if (length(fx) != length(x))
        stop("Function 'f' must first be vectorized: Vectorize(f).")

    if (n == 0) return(f(x))

    if (h == 0) {
        H <- .Machine$double.eps^c(1/3, 1/4, 1/5, 1/6)
        h <- H[n]
    }

    if (method == "central") {
        if (n == 1) {
            df <- (f(x+h) - f(x-h)) / (2*h)
        } else if (n == 2) {
            df <- (f(x+h) - 2*f(x) + f(x-h)) / h^2
        } else if (n == 3) {
            df <- (f(x+2*h) - 2*f(x+h) + 2*f(x-h) - f(x-2*h)) / (2*h^3)
        } else if (n == 4) {
            df <- (f(x+2*h) - 4*f(x+h) + 6*f(x) - 4*f(x-h) + f(x-2*h)) / h^4
        }

    } else if (method == "forward") {
        if (n == 1) {
            df <- (-f(x+2*h) + 4*f(x+h) - 3*f(x)) / (2*h)
        } else if (n == 2) {
            df <- (-f(x+3*h) + 4*f(x+2*h) - 5*f(x+h) + 2*f(x)) / h^2
        } else if (n == 3) {
            df <- (-3*f(x+4*h) + 14*f(x+3*h) - 24*f(x+2*h) + 18*f(x+h) - 5*f(x)) / (2*h^3)
        } else if (n == 4) {
            df <- (-2*f(x+5*h) + 11*f(x+4*h) - 24*f(x+3*h) + 26*f(x+2*h) - 14*f(x+h) + 3*f(x)) / h^4
        }

    } else if (method == "backward") {
        if (n == 1) {
            df <- (3*f(x) - 4*f(x-h) + f(x-2*h)) / (2*h)
        } else if (n == 2) {
            df <- (2*f(x) - 5*f(x-h) + 4*f(x-2*h) - f(x-3*h)) / h^2
        } else if (n == 3) {
            df <- (5*f(x) - 18*f(x-h) + 24*f(x-2*h) - 14*f(x-3*h) + 3*f(x-4*h)) / (2*h^3)
        } else if (n == 4) {
            df <- (3*f(x) - 14*f(x-h) + 26*f(x-2*h) - 24*f(x-3*h) + 11*f(x-4*h) - 2*f(x-5*h)) / h^4
        }
        
    } else
        stop("Unknown 'method'; use 'central', 'forward' or 'backward' instead.")

    return(df)
}
