##
##  f m i n b n d . R
##


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
