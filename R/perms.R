##
##  p e r m s . R  Permutations
##

perms <- function(a) {
    n <- length(a)
    if (length(a) == 0) return(c())
    else    if (n <= 1) return(matrix(a, 1, 1))

    q <- perms(1:(n-1))  # recursive call
    m <- nrow(q)
    P <- matrix(0, n*m, n)
    P[1:m, ] <- cbind(matrix(n, m, 1), q)

    for (i in (n-1):1) {
        t <- q
        t[t == i] <- n
        P[(m*(n-i)+1):(m*(n-i+1)), ] <- cbind(i*matrix(1, m, 1), t)
    }

    b <- a[c(P)]
    dim(b) <- dim(P)
    return(b)
}

randperm <- function(a) {
    n <- length(a)
    if (n == 0)      return(c())
    else if (n <= 1) return(a)

    m <- sample(1:n, size = n, replace = FALSE)
    return(a[m])
}
