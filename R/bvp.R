##
##  b v p . R  Boundary Value Problems
##


bvp <- function(f, a, b, ya, yb, N, cc, ...) {
    stopifnot(is.numeric(a), length(a) == 1, is.numeric(b), length(b) == 1,
              is.numeric(ya), length(ya) == 1, is.numeric(yb), length(yb) == 1)

    if (!is.numeric(N) || length(N) != 1 || floor(N) != ceiling(N) || N < 0)
        stop("Argument 'N' must be an integer greater or equal 1.")
    if (!is.numeric(cc) || length(cc) != 3)
        stop("Argument 'cc' must be a real vector of length 3.")

    fun <- match.fun(f)
    f   <- function(x) fun(x, ...)

    h <- (b-a)/(N+1)
    xh <- linspace(a, b, N+2)
    hm <- cc[1]/h^2
    hd <- cc[2]/(2*h)

    A <- diag((2*hm + cc[3]), N, N)
    A[col(A) == row(A)-1] <- -hm - hd
    A[col(A) == row(A)+1] <- -hm - hd

    F <- f(xh[2:(N+1)])
    F[1] <- F[1] + ya*(hm + hd)
    F[N] <- F[N] + yb*(hm - hd)

    yh <- qr.solve(A, F)
    yh <- c(ya, yh, yb)

    return(list(xh = xh, yh = yh))
}
