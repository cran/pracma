##
##  f m i n u n c . R  Unconstrained Minimization
##

#   o Added 'fminunc()' for unconstrained function minimization, based
#     on a 'variable metric' approach by John Nash (see package Rvmmin).


#' Minimization of unconstrained multivariable functions
fminunc <- function(x0, fn, gr = NULL, ...,
                    tol = 1e-08, maxiter = 0, maxfeval = 0) {
    if (!is.numeric(x0) || length(x0) <= 1)
        stop("Argument 'x0' must be a vector of length greater than 1.")
    fun <- match.fun(fn)
    fn  <- function(x) fun(x, ...)
    if (is.null(gr)) gr <- function(x) pracma::grad(fn, x)

    sol <- .varmetric(x0, fn, gr, tol = tol,
                      maxiter = maxiter, maxfeval = maxfeval)
    return(sol)
}


.varmetric <- function(par, fn, gr, 
                       tol = 1e-08, maxiter = 0, maxfeval = 0) {
    
    # Prepare control parameters
    n <- length(par)
    if (maxiter == 0)  maxiter  <- 1000 +  5 * n
    if (maxfeval == 0) maxfeval <- 3000 + 10 * n
    maxit <- maxiter
    eps <- tol
    acctol <- 0.0001
    dowarn <- FALSE
    stepredn <- 0.2
    offset <- 100.0
    stopbadupdate <- TRUE
    ceps <- .Machine$double.eps * offset
    dblmax <- .Machine$double.xmax  # used to flag bad function
    
    # Set working parameters
    keepgoing <- TRUE
    ifn <- ig <- 1  # count function evaluations
    ilast <- ig
    bvec <- par; f <- fn(bvec)
    fmin <- f;   g <- gr(bvec)
    oldstep <- 1
    msg <- "Status not resolved"
    conv <- -1
    gnorm <- sqrt(sum(g*g))
    if (gnorm < (1 + abs(fmin))*eps*eps ) {
        keepgoing <- FALSE
        msg <- "Small gradient norm"
        conv <- 2
    }
    while (keepgoing) { ## main loop
        if (ilast == ig) B <- diag(1, n, n)
        fmin <- f
        par <- bvec
        c <- g
        t <- as.vector(-B %*% g)        # compute search direction
        gradproj <- sum(t * g)          # gradient projection
        accpoint <- FALSE 
        if (gradproj <= 0) {
            changed <- TRUE
            steplength <- oldstep
            while ((f >= fmin) && changed && (!accpoint)) {
                bvec <- par + steplength * t
                changed <- (!identical((bvec + offset), (par + offset)) )
                if (changed) {
                    f <- fn(bvec)
                    ifn <- ifn + 1
                    if (ifn > maxfeval) {
                        msg <- "Too many function evaluations"
                        if (dowarn) warning(msg)
                        conv <- 1
                        changed <- FALSE
                        keepgoing <- FALSE
                        break
                    }
                    if (f < fmin) {
                        accpoint <- (f <= fmin + gradproj * steplength * acctol)
                    }
                    else {
                        steplength <- steplength * stepredn
                    }
                }
            }
        }
        if (accpoint) {
            fmin <- f
            g <- gr(bvec)
            ig <- ig + 1
            if (ig > maxit) {
                keepgoing = FALSE
                msg = "Too many gradient evaluations"
                if (dowarn) warning(msg)
                conv <- 1
                break
            }
            par <- bvec
            gnorm <- sqrt(sum(g*g)) 
            if (gnorm < (1 + abs(fmin))*eps*eps ) {
                keepgoing <- FALSE
                msg <- "Small gradient norm"
                conv <- 2
                break
            }
            t <- as.vector(steplength * t)
            c <- as.vector(g - c)
            D1 <- sum(t * c)
            if (D1 > 0) {
                y <- as.vector(crossprod(B, c))
                D2 <- as.double(1+crossprod(c,y)/D1)  
                B <- B - (outer(t, y) + outer(y, t) - D2 * outer(t, t))/D1
            } else {
                if (ig == ilast+1) {
                    if (stopbadupdate && ! accpoint) keepgoing=FALSE
                    msg <- paste("UPDATE NOT POSSIBLE: ilast, ig",
                                 ilast, ig, sep = "")
                    conv <- 3
                }
                ilast <- ig
            }
        } else {
            if ( (ig == ilast) || (abs(gradproj) < (1 + abs(fmin))*eps*eps ) ) {
                # we reset to gradient and did new linesearch
                keepgoing <- FALSE  # no progress possible
                if (conv < 0) { # conv == -1 is used to indicate it is not set
                    conv <- 0
                }
                msg <- "Rvmminu converged"
            } else {
                ilast <- ig
            }
        }
    }  # end main loop
    
    counts <- c("function" = ifn, "gradient" = ig)
    ans <- list(par = par, value = fmin, counts = counts,
                convergence = conv, message = msg)
    return(ans) 
}

