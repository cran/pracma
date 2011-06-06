##
##  f z e r o . R
##


fzero <- function(f, x0, ..., maxiter = 100, tol = .Machine$double.eps^(1/2)) {
    if (!is.numeric(x0) || length(x0) > 2)
        stop("Argument 'x0' must be a scalar or a vector of length 2.")

    if (length(x0) == 2) {
        zero <- uniroot(f, x0, ..., tol = tol)
    } else {
        if (x0 != 0) dx <- x0/50
        else         dx <-  1/50
        sqrt2 <- sqrt(2)

        a <- b <- x0
        fa <- fb <- f(x0, ...)
        if (fa == 0) return(list(x = a, fval = fa))

        iter <- 0
        while (fa * fb > 0 && iter < maxiter) {
            iter <- iter + 1
            dx <- sqrt2 * dx
            a  <- a - dx
            fa <- f(a, ...)
            if (fa * fb <= 0) break
            b  <- b + dx
            fb <- f(b, ...)
        }
        if (iter == maxiter) {
            warning("Maximum number of iterations exceeded; no zero found.")
            return(list(x = NA, fval = NA))
        }
        zero <- uniroot(f, c(a, b), ..., tol = tol)
    }
    x.zero <- zero$root
    f.zero <- zero$f.root

    return(list(x = x.zero, fval = f.zero))
}


fminbnd <- function(f, x1, x2, ..., minimize = TRUE,
                                   tol = .Machine$double.eps^(2/3)) {
    if (!is.numeric(x1) || length(x1) != 1 ||
        !is.numeric(x2) || length(x2) != 1)
        stop("Arguments 'x1' and 'x2' must be numeric scalars.")

    if (minimize) {
        fopt <- optimize(f, c(x1, x2), ..., maximum = FALSE, tol = tol)
        return(list(x = fopt$minimum, fval = fopt$objective))
    } else {
        fopt <- optimize(f, c(x1, x2), ..., maximum = TRUE, tol = tol)
        return(list(x = fopt$maximum, fval = fopt$objective))
    }
}

fminsearch <- function(f, x0, ..., minimize = TRUE,
                                   tol = .Machine$double.eps^(2/3)) {
    if (!is.numeric(x0))
        stop("Argument 'x0' must be a numeric vector.")

    scl <- if(minimize) 1 else -1

    fopt <- optim(x0, f, ..., method = "Nelder-Mead",
                  control = list(fnscale = scl, reltol = tol))

    return(list(x = fopt$par, fval = fopt$value))
}


fsolve <- function(f, x0, ...) {
    stop("Function 'fsolve' is not yet implemented in package 'pracma'.")
}
