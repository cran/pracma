###
### p o l y v a l . R  Polynomial Evaluation
###


polyval <- function(p, x) {
    if (length(x) == 0) return(c())
    if (length(p) == 0) return(0 * x)
    if (!is.vector(p, mode="numeric") && !is.vector(p, mode="complex"))
        stop("Argument 'p' must be a real or complex vector.")
    if (!is.vector(x) && !is.matrix(x))
        stop("Argument 'x' must be a real or complex matrix.")

    n <- length(p)
    y <- outer(x[1:length(x)], (n-1):0, "^") %*% p
    dim(y) <- dim(x)
    return(y)
}

polyvalm <- function(p, A) {
    stopifnot(is.numeric(p) || isempty(p), is.numeric(A))
    if (!is.vector(p))
        stop("Argument 'p' must be a numeric vector.")
    if (!is.matrix(A) || nrow(A) != ncol(A))
        stop("Argument 'A' must be a square matrix.")
    
    n <- length(p)
    if (n == 0) {
        y <- zeros(nrow(A))
    } else if (n == 1) {
        y <- diag(p, n)
    } else {
        id <- eye(nrow(A))
        y <- p[1] * id
        for (i in 2:n)
            y <- y %*% A + p[i] * id
    }
    return(y)
}
