##
##  s i c i . R  Sine and cosine integral functions
##


Si <- function(x) {
    stopifnot(is.numeric(x))
    sapply(x, .sici)[1, ]
}

Ci <- function(x) {
    stopifnot(is.numeric(x))
    sapply(x, .sici)[2, ]
}


.sici <- function(x) {
    stopifnot(is.numeric(x), length(x) == 1)
    bj <- numeric(101)
    p2 <- 1.570796326794897     # pi/2
    el <- 0.5772156649015329    # gamma
    epsi <- 1.0e-15
    x2 <- x * x
    if (x >= 0.0) sgnx <- 1L
    else {sgnx <- -1L; x <- sgnx * x}
    # start the computation
    if (x == 0.0) {
        si <- 0.0; ci <- -Inf
    } else if (x <= 16.0) {
        xr <- -0.25 * x2
        ci <- el + log(x) + xr
        for (k in 2:40) {
            xr <- -0.5 * xr * (k-1)/(k*k*(2*k-1)) * x2
            ci <- ci + xr
            if (abs(xr) < abs(ci) * epsi) break
        }
        xr <- x
        si <- x
        for (k in 1:40) {
            xr <- -0.5 * xr * (2*k-1) / k / (4*k*k + 4*k + 1) * x2
            si <- si + xr
            if (abs(xr) < abs(si) * epsi) break
        }
    } else if (x < 32.0) {
        m <- floor(47.2 + 0.82 * x)
        xa1 <- 0.0
        xa0 <- 1e-100
        for (k in m:1) {
            xa <- 4.0 * k * xa0/x - xa1
            bj[k] <- xa
            xa1 <- xa0
            xa0 <- xa
        }
        xs <- bj[1]
        for (k in seq(3, m, by=2)) {
            xs <- xs + 2.0 * bj[k]
        }
        bj[1] <- bj[1] / xs
        for (k in 2:m) {
            bj[k] <- bj[k] / xs
        }
        xr <- 1.0
        xg1 <- bj[1]
        for (k in 2:m) {
            xr <- 0.25 * xr * (2.0*k-3.0)^2 / ((k-1)*(2*k-1)^2) * x
            xg1 <- xg1 + bj[k] * xr
        }
        xr <- 1.0
        xg2 <- bj[1]
        for (k in 2:m) {
            xr <- 0.25 * xr * (2*k-5)^2 / ((k-1)*(2*k-3)^2) * x
            xg2 <- xg2 + bj[k] * xr
        }
        xcs <- cos(x/2.0)
        xss <- sin(x/2.0)
        ci <- el + log(x) - x * xss * xg1 + 2 * xcs * xg2 - 2 * xcs * xcs
        si <- x * xcs * xg1 + 2 * xss * xg2 - sin(x)
    } else {
        xr <- 1.0
        xf <- 1.0
        for (k in 1:9) {
            xr <- -2.0 * xr * k * (2*k-1) / x2
            xf <- xf + xr
        }
        xr <- 1.0/x
        xg <- xr
        for (k in 1:8) {
            xr <- -2.0 * xr * (2*k+1) * k / x2
            xg <- xg + xr
        }
        ci <- xf * sin(x) / x - xg * cos(x) / x
        si <- p2 - xf * cos(x) / x - xg * sin(x) / x
    }
    si <- sgnx * si
    return( c(si, ci) )
}
