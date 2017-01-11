shooting <- function(f, t0, tfinal, y0, h, a, b,
                     itermax = 20, tol = 1e-6, hmax = 0) {
    stopifnot(is.numeric(y0), length(y0) == 1,
              is.numeric(t0), length(t0) == 1,
              is.numeric(tfinal), length(tfinal) == 1,
              is.numeric(a), length(a) == 1,
              is.numeric(b), length(b) == 1)

    fun <- match.fun(f)
    ff  <- function(t, y)
              matrix( c(y[2], fun(t, y[1], y[2])) )
    hun <- match.fun(h)
    hh  <- function(u, v) hun(u, v)

    dy <- numeric(itermax); dy[1:2] <- c(a, b)
    m  <- numeric(itermax)
    test <- 1; i <- 1

    while (test > tol && i <= itermax) {
        if (i > 2)
            dy[i] <- dy[i-1] - (dy[i-1]-dy[i-2]) * m[i-1] / (m[i-1]-m[i-2])
        z0 <- matrix(c(y0, dy[i]))
        sol <- ode45(ff, t0, tfinal, z0, atol = tol, hmax = hmax)
        Tsol <- sol$t; Ysol <- sol$y
        n <- length(Tsol)
        yend <- Ysol[n, 1]; ypend <- Ysol[n, 2]
        m[i] <- h(yend, ypend)
        test <- abs(m[i])
        i <- i+1
    }
    return(list(t = Tsol, y = Ysol))
}
