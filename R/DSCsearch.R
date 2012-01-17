##
## D S C s e a r c h . R  Davies-Swann-Campey Search
##


DSCsearch <- function(f, p, inc = 0.05, scl = 0.1, tol = 1e-6) {
    f <- match.fun(f)
    if (!is.numeric(p) || length(p) != 1)
        stop("Parameter 'p' must be a numeric scalar.", call. = FALSE)

    k  <- 0
    ke <- 0
    dt <- inc
    x0 <- p
    er <- dt

    while (er >= tol) {
        x1 <- x0 + dt
        x_1 <- x0 - dt
        f0 <- f(x0)
        f1 <- f(x1)
        ke <- ke + 2
        if (f0 > f1) {
           p <- 1
           n <- 1
           fn_1 <- f0
           fn <- f1
           xn <- x1
           ind <- 0
        } else {
           f_1 <- f(x_1)
           ke <- ke + 1
           if (f_1 < f0) {
              p <- -1
              n <- 1
              fn_1 <- f0
              fn <- f_1
              xn <- x_1
              ind <- 0
           } else {
              ind <- 1
           }
        }

        if (ind == 0) {
           while (fn <= fn_1) {
                 n <- n + 1
                 fn_2 <- fn_1
                 fn_1 <- fn
                 xn_1 <- xn
                 xn <- xn_1 + (2^(n-1))*p*dt
                 fn <- f(xn)
                 ke <- ke + 1
           }
           xm <- xn_1 - 2^(n-2)*p*dt
           fm <- f(xm)
           ke <- ke + 1
           if (fm >= fn_1) {
              x0 <- xn_1+(2^(n-2)*p*dt*(fn_2-fm))/(2*(fn_2-2*fn_1+fm))
           } else {
              x0 <- xm+(2^(n-2)*p*dt*(fn_1-fn))/(2*(fn_1-2*fm+fn))
           }
           er <- 2^(n-2)*dt
           dt <- scl*dt
        } else {
           x0 <- x0 + dt*(f_1 - f1)/(2*(f_1 - 2*f0 + f1))
           er <- dt
           dt <- scl*dt
        }
        k <- k + 1
    }

    return(list(xmin = x0, fmin = f(x0), niter = k, feval = ke))
}
