broyden <- function(Ffun, x0, maxiter = 20, tol = .Machine$double.eps^(1/2)) {
    if (!is.numeric(x0))
        stop("Argument 'x0' must be a numeric (row or column) vector.")
    F <- match.fun(Ffun)
    y0 <- F(x0)
    if (length(x0) != length(y0))
        stop("Function 'F' must be 'square', i.e. from R^n to R^n .")

    # Compute once the Jacobian and its inverse
    A0 <- jacobian(F, x0)
    B0 <- inv(A0)

    # Secant-like step in Broyden's method
    xnew <- x0 - B0 %*% y0
    ynew <- F(xnew)

    k <- 1
    while (k < maxiter) {
        s <- xnew - x0
        d <- ynew - y0
        if (norm(s, "F") < tol || norm(as.matrix(ynew), "F") < tol)  break

        # Sherman-Morrison formula
        B0 <- B0 + (s - B0 %*% d) %*% t(s) %*% B0 / c(t(s) %*% B0 %*% d)

        # Update for next iteration
        x0 <- xnew
        xnew <- xnew - B0 %*% ynew
        y0 <- ynew
        ynew <- F(xnew)

        k <- k + 1
    }

    if (k >= maxiter)
        warning(paste("Not converged: Max number of iterations reached."))

    fnew <- sqrt(sum(ynew^2))
    return(list(zero = xnew, fnorm = fnew, niter = k))
}
