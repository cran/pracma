##
##  f m i n s e a r c h . R
##


fminsearch <- function(fn, x0, ..., lower = NULL, upper = NULL, 
                       method = c("Nelder-Mead", "Hooke-Jeeves"),
                       minimize = TRUE, maxiter = 1000, tol = 1e-08) {

    n <- length(x0)
    if (!is.numeric(x0) || n == 0) {
        stop("Argument 'x0' must be a numeric vector.")
	} else if (n == 1) {
		stop("Don't use 'fminsearch' for one-dimensional minimization.")
	}

    method <- match.arg(method)

    scl <- if(minimize) 1 else -1
    fun <- match.fun(fn)
    fn <- function(x) scl * fun(x, ...)

    if ((!is.null(lower) || !is.null(upper)) && method == "Nelder-Mead") {
        stop("'Nelder-Mead' cannot handle bounds constraints;\n",
                "Instead, use 'Hooke-Jeeves' with bounds/box constraints.")
    }

    if (method == "Nelder-Mead") {
        # Call Nelder-Mead w/o bounds constraints
        opt <- nelder_mead(fn, x0, tol = tol, maxfeval = 5*maxiter)

    } else if (method == "Hooke-Jeeves") {
        if (is.null(lower) && !is.null(upper)) lower <- rep(-Inf, n)
        if (!is.null(lower) && is.null(upper)) upper <- rep( Inf, n)

        # Call Hooke-Jeeves w/o or w/ bounds constraints
        opt <- hooke_jeeves(x0, fn, lb = lower, ub = upper,
                            tol = tol, maxfeval = 10*maxiter)
    } else {
        warning("Unknown Method: use 'Nelder-Mead' or 'Hooke-Jeeves'!")
    }

    xopt <- opt$xmin; fopt <- opt$fmin
    if (! minimize) fopt <- -fopt
    return(list(xmin = xopt, fmin = fopt, count = opt$count,
                convergence = 0, info = opt$info))
}
