##
##  s t e r e o g r a p h i c  Stereographic Projection
##


stereographic <- function(p) {
    stopifnot(is.numeric(p))
    if (is.vector(p)) p <- as.matrix(p)
    m <- nrow(p); n <- ncol(p)

    s <- 2.0 / ( 1.0 + p[m, 1:n] )
    ss <- repmat(s, m, 1)
    f <- zeros(m,1)
    f[m] <- -1.0
    ff <- repmat ( f, 1, n )

    q <- ss * p + (1.0 - ss) * ff
    return(q)
}

stereographic_inv <- function(q) {
    stopifnot(is.numeric(q))
    if (is.vector(q)) q = as.matrix(q)
    m <- nrow(q); n <- ncol(q)

    f <- zeros(m, 1)
    f[m] <- -1.0
    ff <- repmat(f, 1, n)

    s <- 4.0 / (4.0 + apply(q[1:(m-1),1:n, drop = FALSE]^2, 2, sum))
    ss <- repmat (s, m, 1)

    p <- ss * q + (1.0 - ss) * ff[1:m,1:n]
    return(p)
}
