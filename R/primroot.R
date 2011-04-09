##
##  p r i m r o o t . R  Primitive Root
##


modpower <- function(n, k, m) {
    stopifnot(is.numeric(n), floor(n) == ceiling(n), n >= 0,
              is.numeric(k), floor(k) == ceiling(k), n >= 0,
              is.numeric(m), floor(m) == ceiling(m), m >= 0)

    if (k == 0) return(1)
    r <- n %% m; k <- k-1
    while (k > 0) {
        r <- (r*n) %% m; k <- k-1
    }
    return(r)
}

modorder <- function(n, m) {
    stopifnot(is.numeric(n), floor(n) == ceiling(n), n >= 0,
              is.numeric(m), floor(m) == ceiling(m), m >= 0)

    if (!coprime(n, m)) return(0)
    r <- n %% m; k <- 1
    if (r == 0) return(NA)
    while (r != 1) {
        r <- (n*r) %% m; k <- k + 1
    }
    return(k)
}

primroot <- function(m) {
    stopifnot(is.numeric(m), floor(m) == ceiling(m), m >= 0)

    if (!isprime(m)) return(NA)
    if (m == 2) return(1)

    ##  Brute Force:
    for (r in 2:(m-1)) {
        k <- modorder(r, m)
        if (k == m-1) break
    }
    return(r)

    ##  Factorize (m-1):
    # P <- unique(ifactor(m-1))
    # for (r in 2:(m-1)) {
    #     x <- TRUE
    #     for (p in P) {
    #         if (modpower(r, (m-1)/p, m) == 1) x <- FALSE
    #     }
    #     if (isTRUE(x)) return(r)
    # }
    # stop("No prim root found.")
}
