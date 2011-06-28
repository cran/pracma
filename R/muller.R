##
##  m u l l e r . R  Muller's Method
##


muller <- function(f, x1, x2, tol = .Machine$double.eps^0.5, kmax = 24) {
    stopifnot(is.numeric(x1) || is.complex(x1), length(x1) == 1,
              is.numeric(x2) || is.complex(x2), length(x2) == 1)
    f <- match.fun(f)
    .sign <- function(z)
        if (z == 0) 0 else z/abs(z)

    x <- c(  x1,    x2,    (x1+x2)/2)
    y <- c(f(x1), f(x2), f((x1+x2)/2))

    a <- b <- numeric(kmax)
    a[1] <- (y[2] - y[1])/(x[2] - x[1])

    for (i in 3:kmax) {
        a[i-1] <- (y[i] - y[i-1])/(x[i] - x[i-1])
        b[i-2] <- (a[i-1] - a[i-2])/(x[i] - x[i-2])

        s <- a[i-1] + (x[i] - x[i-1])*b[i-2]

        x[i+1] <- x[i] - 2*y[i]/(s + .sign(s)*sqrt(s^2 - 4*y[i]*b[i-2]))
        y[i+1] <-  f(x[i+1])

        if (abs(x[i+1] - x[i]) < tol) break
        j <- i + 1
    }
    if (j > kmax) {
        j <- j-1
        warning("Root not found to the desired tolerance.")
    }

    return(list(root = x[j+1], fval = y[j+1]))
}
