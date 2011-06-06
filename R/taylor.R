##
##  t a y l o r . R  Taylor Series Approximation
##


taylor <- function(f, x0, n = 4, ...) {
    if (!is.numeric(x0))
        stop("Argument 'x0' must be a numeric value or vector.")
    if (!is.numeric(n) || floor(n) != ceil(n) || length(n) != 1)
        stop("Order 'n' must be an integer between 1 and 4.")

    fun <- match.fun(f)
    f <- function(x) fun(x, ...)

    if (n > 4) {
        n <- 4
        warning("Order 'n' is too high; will be set to 'n=4'.")
    } else if (n <= 0) {
        n <- 1
        warning("Order 'n' is too low; will be set to 'n=1'.")
    }

    T <- f(x0)
    for (i in 1:n) {
        T <- polyadd(T, fderiv(f, x0, i)/fact(i) * polypow(c(1, -x0), i))
    }
    return(T)
}
