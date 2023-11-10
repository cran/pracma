##
##  b v p . R  Boundary Value Problems
##


bvp <- function(f, g, h, x, y, n = 50) {
    stopifnot(is.numeric(x), is.numeric(y), is.numeric(n))
    if (length(x) != 2 || length(y) != 2)
        stop("Arguments 'x' and 'y' must have length 2.")
    if (length(n) != 1 || floor(n) != ceiling(n) || n < 2)
        stop("Argument 'n' must be an integer greater or equal 2.")

    if (is.numeric(f)) ffun <- function(x) rep(f[1], length(x))
    else               ffun <- match.fun(f)
    if (is.numeric(g)) gfun <- function(x) rep(g[1], length(x))
    else               gfun <- match.fun(g)
    if (is.numeric(h)) hfun <- function(x) rep(h[1], length(x))
    else               hfun <- match.fun(h)

    xa <- x[1]; xb <- x[2]
    ya <- y[1]; yb <- y[2]
    xs <- linspace(xa, xb, n+2)[2:(n+1)]
    dt <- (xb - xa) / (n+1)

    a <- -2 - dt^2 * gfun(xs)           # main diagonal
    b <-  1 - dt/2 * ffun(xs[1:(n-1)])  # superdiagonal
    d <-  1 + dt/2 * ffun(xs[2:n])      # subdiagonal

    rhs <- dt^2 * hfun(xs)              # right hand side
    rhs[1] <- rhs[1] - ya * (1 + (dt/2) * ffun(xs[1]))
    rhs[n] <- rhs[n] - yb * (1 - (dt/2) * ffun(xs[n]))

    ys <- trisolve(a, b, d, rhs)
    return(list(xs = c(xa, xs, xb), ys = c(ya, ys, yb)))
}


# bvp <- function(p, q, r, a, b, ya, yb) {
#     z0 <- as.matrix(c(ya, 0, 0, 1))
#     fun0 <- function(x, z) {
#         as.matrix(c(z[2],
#                     p(x)*z[2] + q(x)*z[1] + r(x),
#                     z[4],
#                     p(x)*z[4] + q(x)*z[3]
#                    )
#         )
#     }
#     res <- ode45(fun0, a, b, z0, hmax = 0.05)
#     t <- res$t; z <- res$y
#     n <- length(t)
# 
#     y <- z[, 1] + (yb - z[n, 1]) * z[, 3] / z[n, 3]
#     return(list(xs = t, ys = y))
# }
