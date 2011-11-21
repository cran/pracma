##
##  f z e r o . R
##


fzero <- function(f, x0, ..., maxiter = 100, tol = .Machine$double.eps^(1/2)) {
    if (!is.numeric(x0) || length(x0) > 2)
        stop("Argument 'x0' must be a scalar or a vector of length 2.")

    if (length(x0) == 2) {
        zero <- zeroin(f, x0, ..., tol = tol)
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
        zero <- zeroin(f, c(a, b), ..., tol = tol)
    }
    x.zero <- zero$root
    f.zero <- zero$f.root

    return(list(x = x.zero, fval = f.zero))
}


zeroin <- function(f, interval, ..., tol = .Machine$double.eps^(1/2)) {
    if (!is.numeric(interval) || length(interval) != 2)
        stop("Argument 'interval' must be a numeric vector of length 1.")

    a <- interval[1]; fa <- f(a, ...); nf <- 1
    if (fa == 0)
        return(list(root = a, f.root = fa, f.calls = nf, estim.prec = 0.0))
    b <- interval[2]; fb <- f(b, ...); nf <- nf + 1
    if (fb == 0)
        return(list(root = b, f.root = fb, f.calls = nf, estim.prec = 0.0))
    if (sign(fa) == sign(fb))
        stop("Function 'f' must change sign on the interval.")

    # Initialize
    cc <- a
    fc <- fa
    d  <- b - cc
    e <- d

    # Main loop
    while (fb != 0) {
        if (sign(fa) == sign(fb)) {
           a <- cc; fa <- fc
           d <- b - cc;  e <- d
        }
        if (abs(fa) < abs(fb)) {
           cc <- b;  b <- a;   a <- cc
           fc <- fb; fb <- fa; fa <- fc
        }

        # Convergence test and possible exit
        m <- 0.5 * (a - b)
        tol <- 2.0 * tol * max(abs(b), 1.0)
        if (abs(m) <= tol || (fb == 0.0)) {
           break
        }

        # Choose bisection or interpolation
        if (abs(e) < tol || abs(fc) <= abs(fb)) {
           # Bisection
           d <- m
           e <- m
        } else {
           # Interpolation
           s <- fb/fc
           if (a == cc) {
              # Linear interpolation (secant)
              p <- 2.0 * m * s
              q <- 1.0 - s
           } else {
              # Inverse quadratic interpolation
              q <- fc/fa
              r <- fb/fa
              p <- s * (2.0 * m * q * (q - r) - (b - cc) * (r - 1.0))
              q <- (q - 1.0) * (r - 1.0) * (s - 1.0)
           }

           if (p > 0) q <- -q else p <- -p
           # Is interpolated point acceptable
           if (2.0*p < 3.0*m*q - abs(tol*q) && p < abs(0.5*e*q)) {
              e <- d
              d <- p/q
           } else {
              d <- m
              e <- m
           }
        }

        # Next point
        cc <- b
        fc <- fb
        if (abs(d) > tol) {
           b <- b + d
        } else {
           b <- b - sign(b-a) * tol
        }
        fb <- f(b, ...); nf <- nf + 1
    }

    return(list(root = b, f.root = fb, f.calls = nf, estim.prec = m))
}
