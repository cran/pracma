##
##  i n t e g r a l . R  Numerical Integration
##


integral <- function(fun, xmin, xmax,
                method = c("Kronrod", "Clenshaw","Simpson"),
                no_intervals = 8, random = FALSE,
                reltol = 1e-8, abstol = 0, ...)
{
    stopifnot(is.numeric(xmin), length(xmin) == 1,
              is.numeric(xmax), length(xmax) == 1)
    no_intervals <- max(1, floor(no_intervals))

    fun <- match.fun(fun)
    f <- function(x) fun(x, ...)

    if (length(f(xmin)) > 1 || length(f(xmax)) > 1) {
        stop("Function 'fun' is array-valued! Use 'quadv'.\n")
    }
    
    if (length(f(c(xmin, xmax))) != 2) {
        cat("Warning: Function 'fun' is not vectorized!\n")
        f = Vectorize(f)
    }

    if (xmin == xmax) return(0)
    method <- match.arg(method)
    tol <- if (abstol > 0) min(reltol, abstol) else reltol

    if (is.infinite(xmin) || is.infinite(xmax)) {
        cat("For infinite domains Gauss integration is applied!\n")
        Q <- quadinf(f, xmin, xmax, tol = tol)$Q
        return(Q)
    }

    if (random) {
        xs <- c(xmin,
                (xmax - xmin)*sort(runif(no_intervals - 1)) + xmin,
                xmax)
    } else {
        xs <- linspace(xmin, xmax, no_intervals + 1)
    }

    # Q <- switch(method,
    #         "Kronrod"    = quadgk(f, xmin, xmax, tol = tol),
    #         "Clenshaw"   = quadcc(f, xmin, xmax, tol = tol),
    #         "Simpson"    = simpadpt(f, xmin, xmax, tol = tol)
    #         )
    Q <- 0
    if (method == "Kronrod") {
        for (i in 1:no_intervals) {
            Q = Q + quadgk(f, xs[i], xs[i+1], tol = tol)
        }
    } else if (method == "Clenshaw") {
        for (i in 1:no_intervals) {
            Q = Q + quadcc(f, xs[i], xs[i+1], tol = tol)
        }
    } else if (method == "Simpson") {
        for (i in 1:no_intervals) {
            Q = Q + simpadpt(f, xs[i], xs[i+1], tol = tol)
        }
    } else {
        stop("Unknown method; not available as integration routine.")
    }

    return(Q)
}


line_integral <- function (fun, waypoints, method = NULL, reltol = 1e-8, ...) {
    stopifnot(is.complex(waypoints) || is.numeric(waypoints),
              is.null(method) || is.character(method))

    if (length(waypoints) <= 1) return(0 + 0i)

    fun <- match.fun(fun)
    f <- function(z) fun(z, ...)

    Q <- 0 + 0i
    for (i in 2:length(waypoints)) {
        a <- waypoints[i-1]
        b <- waypoints[i]
        d <- b - a

        f1 <- function(t) Re(f(a + t*d))
        f2 <- function(t) Im(f(a + t*d))

        if (is.null(method)) {
            Qre <- integrate(f1, 0, 1, subdivisions = 300L, rel.tol = reltol)$value
            Qim <- integrate(f2, 0, 1, subdivisions = 300L, rel.tol = reltol)$value
        } else {
            Qre <- integral(f1, 0, 1, reltol = reltol, method = method)
            Qim <- integral(f2, 0, 1, reltol = reltol, method = method)
        }
        Q <- Q + d * (Qre + Qim*1i)
    }

    return(Q)
}

