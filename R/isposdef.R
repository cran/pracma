isposdef <- function(A, psd = FALSE, tol = 1e-10) {
    if (nrow(A) != ncol(A)) {
        warning("Matrix 'A' is not quadratic.\n", .call = FALSE)
        a <- FALSE
    } else if (any(abs(A - t(A)) > tol)) {
        warning("Matrix 'A' is not symmetric.\n", .call = FALSE)
        a <- FALSE
    } else {
        e <- try(chol(A, pivot = psd), silent = TRUE)
        if(inherits(e, "try-error")) {
            a <- FALSE
        } else {
            a <- TRUE
        }
    }
    return(a)
}
