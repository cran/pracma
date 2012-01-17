##
##  k m e a n s p p . R  K-means++ Clustering
##


kmeanspp <- function(X, k) {
    stopifnot(is.numeric(X), is.numeric(k))
    if (is.vector(X)) X <- as.matrix(X)
    n <- nrow(X); m <- ncol(X)
    if (floor(k) != ceiling(k) || k < 1 || k > nrow(X))
        stop("Argument 'k' must be a natural number between 1 and nrow(X)")

    C <- numeric(k)
    C[1] <- sample(1:n, 1)

    for (i in 2:k) {
        dm <- distmat(X, X[C, ])
        pr <- apply(dm, 1, min); pr[C] <- 0
        C[i] <- sample(1:n, 1, prob = pr)
    }

    kmeans(X, X[C, ])
}
