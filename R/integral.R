##
##  i n t e g r a l . R  Numerical Integration
##


integral <- function(fun, xmin, xmax,
                method = c("Kronrod","Richardson","Clenshaw","Simpson","Romberg"),
                vectorized = TRUE, arrayValued = FALSE, waypoints = NULL,
                reltol = 1e-8, abstol = 0, ...)
{
    stopifnot(is.numeric(xmin) || is.complex(xmin), length(xmin) == 1,
              is.numeric(xmax) || is.complex(xmax), length(xmax) == 1)
    fun <- match.fun(fun)
    f <- function(x) fun(x, ...)
    # if (!vectorized(f))       # check with those routines called
    #     f <- Vectorize(f)

    if (xmin == xmax) return(0)
    method <- match.arg(method)
    tol <- reltol               # abstol not used

    if (is.complex(xmin) || is.complex(xmax) || !is.null(waypoints)) {
        Q <- cintegral(f, waypoints = as.complex(c(xmin, waypoints, xmax)),
                        method = method, tol = reltol)

    } else if (arrayValued) {
        if (method != "Simpson")
            warning("Only method 'Simpson' available for array-valued functions.")
        Q <- quadv(f, xmin, xmax, tol = tol)$Q

    } else if (is.infinite(xmin) || is.infinite(xmax)) {
        Q <- quadinf(f, xmin, xmax, tol = tol, method = method)

    } else {
        Q <- switch(method,
                "Kronrod"    = quadgk(f, xmin, xmax, tol = tol),
                "Clenshaw"   = quadcc(f, xmin, xmax, tol = tol),
                "Richardson" = quadgr(f, xmin, xmax, tol = tol)$value,
                "Romberg"    = romberg(f, xmin, xmax, tol = tol)$value,
                "Simpson"    = simpadpt(f, xmin, xmax, tol = tol)
                )
    }
    Q <- Q
    return(Q)
}


cintegral <- function (fun, waypoints, method = NULL, reltol = 1e-6, ...) {
    stopifnot(is.complex(points) || is.numeric(points))
    if (length(points) <= 1) return(0 + 0i)

    fun <- match.fun(fun)
    f <- function(z) fun(z, ...)

    Q <- 0 + 0i
    for (i in 2:length(waypoints)) {
        a <- waypoints[i-1]
        b <- waypoints[i]
        d <- b - a

        f1 <- function(t) Re(f(a + t*d))
        f2 <- function(t) Im(f(a + t*d))

        Qre <- integral(f1, 0, 1, reltol = reltol, method = method)
        Qim <- integral(f2, 0, 1, reltol = reltol, method = method)
        Q <- Q + d * (Qre + Qim*1i)
    }

    return(Q)
}
