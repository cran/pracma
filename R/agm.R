##
##   a g m . R  Arithmetic-geometric Mean
##


agm <- function(a, b, maxiter = 25, tol = .Machine$double.eps^(1/2)) {
    stopifnot(is.numeric(a), length(a) == 1, a >= 0,
              is.numeric(b), length(b) == 1, b >= 0)

    niter <- 0
    while (abs(a-b) >= tol && niter <= maxiter) {
        a1 <- (a + b) / 2
        b1 <- sqrt(a * b)
        a <- a1
        b <- b1
        niter <- niter + 1
    }
    if (niter > maxiter)
        warning("Maximum number of allowed iterations exceeded.")
    return(list(agm=(a+b)/2, niter = niter, estim.prec = abs(a-b)))
}
