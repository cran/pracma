##
##  r o m b e r g . R  Romberg Integration, Gaussian Quadrature
##


romberg <- function(f, a, b, kmax = 16, tol = .Machine$double.eps^(2/3), ...)
{
    stopifnot(is.numeric(a), is.numeric(b),
              length(a) == 1, length(b) == 1, a < b)

    fun <- match.fun(f)
    f <- function(x) fun(x, ...)

    trap <- function(f, a, b, n) {
        h <- (b-a) / n
        S <- f(a)
        if (n > 1) {
            for (i in 1:(n-1)) { xi <- a + h * i; S <- S + 2*f(xi) }
        }
        return((S + f(b)) * h/2)
    }

    Q <- matrix(0, nrow = kmax+1, ncol = kmax+1)
    k <- 1
    n <- 2^k
    Q[k+1, 1] <- trap(f, a, b, n)
    Q[1, 2]   <- (4*Q[2, 1] - Q[1, 1])/3

    for (k in 2:kmax) {
       n <- 2^k
       Q[k+1, 1] <- trap(f, a, b, n)
       for (j in 2:(k+1)) {
          c = 4^(j-1)
          Q[k-j+2, j]  = (c*Q[k-j+3, j-1] - Q[k-j+2, j-1]) / (c-1)  
       }
       rel.error <- abs(Q[1, k+1] - Q[1, k]) / Q[1, k+1]
       if (rel.error < tol) break
    }
    if (k == kmax)
        warning("Maximum number of iterations has been reached.")

    return(list(Q = Q[1, j], rel.error = rel.error,j = j))
}


quadQK15 <- function(f, a, b, ...) {
    stopifnot(is.numeric(a), is.numeric(b),
              length(a) == 1, length(b) == 1, a < b)

    fun <- match.fun(f)
    f <- function(x) fun(x, ...)

    eis <- c(2, 4, 6, 8, 10, 12, 14)
    t15 <- c(-0.9914553711208126, -0.9491079123427585, -0.8648644233597691,
             -0.7415311855993944, -0.5860872354676911, -0.4058451513773972,
             -0.2077849550078985,  0.0,                 0.2077849550078985,
              0.4058451513773972,  0.5860872354676911,  0.7415311855993944,
              0.8648644233597691,  0.9491079123427585,  0.9914553711208126)
    t7  <- t15[eis]
    
    c15 <- c(0.02293532201052922, 0.06309209262997855,  0.1047900103222502,
             0.1406532597155259,  0.1690047266392679,   0.1903505780647854,
             0.2044329400752989,  0.2094821410847278,   0.2044329400752989,
             0.1903505780647854,  0.1690047266392679,   0.1406532597155259,
             0.1047900103222502,  0.06309209262997855,  0.02293532201052922)
    c7  <- c(0.1294849661688697,  0.2797053914892767,   0.3818300505051189,
             0.4179591836734694,
             0.3818300505051189,  0.2797053914892767,   0.1294849661688697)

    x15 <- 0.5 * ((b - a) * t15 + b + a)
    x7  <- 0.5 * ((b - a) * t7  + b + a)
    y15 <- f(x15)
    y7  <- f(x7)

    G7  <- sum(c7 * y7)
    K15 <- sum(c15 * y15)

    return(list(value = G7 * (b-a)/2, abs.error = abs(G7 - K15)))
}























