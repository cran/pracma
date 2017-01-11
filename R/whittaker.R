##
##  w h i t t a k e r . R  Whittaker Smoothing
##


whittaker <- function(y, lambda = 1600, d = 2){
    #   Smoothing with a finite difference penalty
    #   y:      signal to be smoothed
    #   lambda: smoothing parameter (rough 50..1e4 smooth)
    #   d:      order of differences in penalty (generally 2)
    
    m <- length(y)
    E <- eye(m)
    D <- diff(E, lag = 1, differences = d)
    B <- E + (lambda * t(D) %*% D)
    z <- solve(B, y)

    return(z)
}

# whittaker <- function(y, lambda = 1600, d = 2) {
#   stopifnot(is.numeric(y))
#   success <- library("SparseM", pos = "package:base",
#                      logical.return = TRUE, warn.conflicts = FALSE)
#   if (!success)
#       stop("Function 'whittaker' requires package 'SparseM'.")
# 
#     m <- length(y)
#     E <- as(m, "matrix.diag.csr")
#     class(E) <- "matrix.csr"
# 
#     Dmat <- diff(E, differences = d)
#     B <- E + (lambda * t(Dmat) %*% Dmat)
#     z <- solve(B, y)
# 
#     return(z)
# }
