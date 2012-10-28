##
##  h i s t c . R  Histogram Count
##


histc <- function(x, edges) {
    stopifnot(is.numeric(x), is.numeric(edges))

    edges <- c(edges)
    n <- length(edges)
    if (is.unsorted(edges))
        stop("Argument 'edges' must be a monotonically non-decreasing vector.")

    if (length(edges) == 1) {
        bin <- numeric(length(x))
        if (is.matrix(x)) dim(bin) <- c(n, ncol(x))
    	return(list(cnt = 0, bin = bin))
    }

    bin <- numeric(length(x))
    if (is.vector(x)) {
        cnt <- numeric(n)
        for (i in 1:(n-1)) {
            li <- edges[i] <= x & x < edges[i+1]
            cnt[i] <- sum(li)
            bin[li] <- i
        }
        li <- x == edges[n]
        cnt[n] <- sum(li)
        bin[li] <- n

    } else if (is.matrix(x)) {
        cnt <- matrix(0, n, ncol(x))
        for (i in 1:(n-1)) {
            li <- edges[i] <= x & x < edges[i+1]
            cnt[i, ] <- apply(li, 2, sum)
            bin[li] <- i
        }
        li <- x == edges[n]
        cnt[n, ] <- apply(li, 2, sum)
        bin[li] <- n

    } else {
        stop("Argument 'x' must be a numeric vector or matrix.")
    }

    dim(bin) <- dim(x)
    return(list(cnt = cnt, bin = bin))
}
