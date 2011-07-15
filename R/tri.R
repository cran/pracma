##
##  t r i . R  Triangular matrices
##


tril <- function(M, k = 0) {
    if (k == 0) {
        M[upper.tri(M, diag = FALSE)] <- 0
    } else {
        M[col(M) >= row(M) + k + 1] <- 0
    }
    return(M)
}


triu <- function(M, k = 0) {
    if (k == 0) {
        M[lower.tri(M, diag = FALSE)] <- 0
    } else {
        M[col(M) <= row(M) + k - 1] <- 0
    }
    return(M)
}
