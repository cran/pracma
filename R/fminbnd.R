##
##  f m i n b n d . R
##


fminbnd <- function(f, a, b, ..., maxiter = 1000, maximum = FALSE,
                    tol = .Machine$double.eps^(2/3)) {
    stopifnot(is.numeric(a), length(a) == 1,
              is.numeric(b), length(b) == 1)
    if (a >= b)
        stop("Interval end points must fulfill  a < b !")

    fun <- match.fun(f)
    if (maximum)
        f <- function(x) -fun(x, ...)
    else
        f <- function(x)  fun(x, ...)

    # phi is the square of the inverse of the golden ratio.
    phi <- 0.5 * ( 3.0 - sqrt ( 5.0 ) )

    # Set tolerances
    eps  <- .Machine$double.eps
    tol1 <- 1 + eps
    eps  <- sqrt(eps)
    tol3 <- tol / 3

    sa <- a; sb <- b
    x  <- sa + phi * ( b - a )
    fx <- f(x)
    v  <- w  <- x
    fv <- fw <- fx
    e  <- 0.0;

    niter <- 1
    while ( niter <= maxiter ) {
        xm  <- 0.5 * ( sa + sb )
        t1 <-  eps * abs ( x ) + tol/3
        t2  <- 2.0 * t1

        #  Check the stopping criterion.
        if ( abs ( x - xm ) <= t2 - (dx <- ( sb - sa ) / 2 ) ) break

        r <- 0.0
        p <- q <- r

        # Fit a parabola.
        if ( t1 < abs ( e ) ) {
            r <- ( x - w ) * ( fx - fv )
            q <- ( x - v ) * ( fx - fw )
            p <- ( x - v ) * q - ( x - w ) * r
            q <- 2.0 * ( q - r );
            
            if ( 0.0 < q ) p <- - p
            
            q <- abs ( q )
            r <- e
            e <- d
        }

        # Is the parabola acceptable
        if ( abs ( p ) < abs ( 0.5 * q * r ) &&
                 q * ( sa - x ) < p          &&
                 p < q * ( sb - x ) ) {
             #  Take the parabolic interpolation step.
             d <- p / q
             u <- x + d

             #  F must not be evaluated too close to a or b.
             if ( ( u - sa ) < t2 | ( sb - u ) < t2 ) {
                 d <- if (x < xm) t1 else -t1
             }

         } else {
         #  A golden-section step.
            e <- if (x < xm) sb - x else a - x
            d <- phi * e
        }

        #  F must not be evaluated too close to X.
        if ( t1 <= abs ( d ) ) {
            u = x + d
        } else if ( 0.0 < d ) {
            u = x + t1
        } else {
            u = x - t1
        }

        fu = f ( u )

        #  Update a, b, v, x, and x.
        if ( fu <= fx ) {
            if ( u < x ) sb <- x
            else         sa <- x
        
            v <- w; fv <- fw
            w <- x; fw <- fx
            x <- u; fx <- fu

        } else {
            if ( u < x ) sa <- u
            else         sb <- u

            if ( fu <= fw || w == x ) {
                v <- w; fv <- fw
                w <- u; fw <- fu
            } else if ( fu <= fv || v == x || v== w ) {
                v <- u; fv <- fu
            }
        }
        niter <- niter + 1

    } #endwhile

    if (niter > maxiter)
        warning("No. of max. iterations exceeded; no convergence reached.")

    if (maximum) fx <- -fx
    return( list(xmin = x, fmin = fx, niter = niter, estim.prec = dx) )
}


# fminbnd <- function(f, x1, x2, ..., minimize = TRUE,
#                                    tol = .Machine$double.eps^(2/3)) {
#     if (!is.numeric(x1) || length(x1) != 1 ||
#         !is.numeric(x2) || length(x2) != 1)
#         stop("Arguments 'x1' and 'x2' must be numeric scalars.")
# 
#     if (minimize) {
#         fopt <- optimize(f, c(x1, x2), ..., maximum = FALSE, tol = tol)
#         return(list(x = fopt$minimum, fval = fopt$objective))
#     } else {
#         fopt <- optimize(f, c(x1, x2), ..., maximum = TRUE, tol = tol)
#         return(list(x = fopt$maximum, fval = fopt$objective))
#     }
# }
