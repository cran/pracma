midpoint <- function(f, t0, tfinal, y0, tol = 1e-08, kmax = 101, ...){

    stopifnot (is.function(f),
               is.numeric(t0), length(t0) == 1,
               is.numeric(tfinal), length(tfinal) == 1,
               is.numeric(y0))
    if (is.vector(y0)) {
        y0 <- as.matrix(y0)
    } else if (is.matrix(y0)) {
        if (ncol(y0) != 1)
            stop ( "midpoint: Argument 'y0' must be row or column vector.")
    }

    fun <- match.fun(f)
    f <- function(t, y) fun (t, y, ...)

    n <- kmax
    y_length <- length(y0)
    y <- y0
    yout <- matrix (NA, n, y_length)
    yout[1, ] <- c(y0)
    
    dt <- (tfinal - t0) / (n-1)
    t <- 0.0
    tw <- 0.0
    ts <- linspace (t0, tfinal, n)

    myfun <- function(w) (w - y) - 0.5*dt*f(tw, w)

    f_length <- length(f(t0, y0))
    if (f_length != y_length)
        stop ("midpoint: Function f must have the same length as 'y0'.") 
    
    #  Choose solver used for root finding
    if (y_length == 1) {
        solver <- fzero
    } else {
        solver <- fsolve
    }
    for (i in 2:n) {
        t <- ts[i-1]
        tw <- t + 0.5 * dt
        w <- y + 0.5 * dt * f(t, y)
        w <- solver ( myfun, w)$x
        w <- 2.0 * w - yout[i-1, ]
        yout[i, ] <- w
        y <- w
    }

    if ( y_length == 1 ) {
        yout <- drop(yout)
    }

    return ( list(t = ts, y = yout))
}



.midp <- function(f, x0, xfinal, y0, nsteps) {
    # stopifnot(nsteps >= 2, x0 < xfinal, f(...) column vector)
    h <- (xfinal - x0) / nsteps
    x <- x0
    y1 <- y0
    y2 <- y1 + h * f(x, y1)

    for (i in 1:(nsteps-1)) {
        x <- x + h
        yy <- y1 + 2.0 * h * f(x, y2)
        y1 <- y2
        y2 <- yy
    }
    0.5*(y1 + y2 + h * f(x, yy))
}


.midpoint <- function(f, t0, tfinal, y0, tol = 1e-07, kmax = 25) {
    stopifnot(is.numeric(y0), is.numeric(t0), length(t0) == 1,
              is.numeric(tfinal), length(tfinal) == 1)
    n <- length(y0)

    # Richardson extrapolation
    nsteps <- 2
    r <- matrix(NA, kmax, n)
    r[1, ] <- .midp(f, t0, tfinal, y0, nsteps)
    rold <- r[1, ]
    for (k in 2:kmax) {
        nsteps <- 2*nsteps
        r[k, 1:n] <- .midp(f, t0, tfinal, y0, nsteps)
        for (j in (k-1):1) {
            cc <- 4^(k-j)
            r[j, ] <- (cc * r[j+1, ] - r[j, ]) / (cc - 1.0)
        }
        dr <- r[1, ] - rold
        err <- sqrt(dot(dr, dr)/n)
        if (err < tol) break
        rold <- r[1, ]
    }
    return(r[1, ])
}


bulirsch_stoer <- function(f, t, y0, tol = 1e-08, ...) {
    stopifnot(is.numeric(t), is.numeric(y0))

    fun <- match.fun(f)
    f <- function(t, y) fun(t, y, ...)   # as row vector

    x <- t
    D <- length(f(x[1], y0))    # image of f
    if (length(y0) != D)
        stop("Argument 'y0' and 'f(..)' must be of the same length.")
    N <- length(x)              # no. of grid points
    if (N < 2)
        stop("Argument 't' must be a vector of length at least 2.")
    mtol <- tol / (N-1)

    z <- matrix(0, N, D)
    z[1, ] <- y0
    for (i in 2:N) {
        z[i, ] <- .midpoint(f, x[i-1], x[i], z[i-1, ], tol = mtol)
    }
    return(z)
}
