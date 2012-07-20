##
##  n e l d e r _ m e a d . R  Nelder-Mead Minimization Algorithm
##


nelder_mead <- function(x0, f, maxiter = 1000, scale = 1,
                        tol = .Machine$double.eps^(2/3), show = FALSE, ...) {

    if (!is.numeric(x0) || length(x0) < 2)
        stop("Argument 'x0' must be a numeric vector of length greater 1.")

    fun <- match.fun(f)
    F   <- function(x) scale * fun(x, ...)

    n <- length(x0)
    # simplex vertices around x0
    V <- t(1/(2*n) * cbind(diag(n), rep(-1, n)) + x0)

    if (show) {
        P <- Q <- c()
    }

    # Function values at vertices
    Y <- numeric(n+1)
    for (j in 1:(n+1)) Y[j] <- F(V[j, ])
    ho <- lo <- which.min(Y)
    li <- hi <- which.max(Y)

    for (j in 1:(n+1)) {
       if (j != lo && j != hi && Y[j] <= Y[li]) li <- j
       if (j != hi && j != lo && Y[j] >= Y[ho]) ho <- j
    }

    cnt <- 0
    while ( Y[hi] > Y[lo] + tol && cnt < maxiter ) {
        S <- numeric(n)
        for (j in 1:(n+1)) S <- S + V[j,1:n]
        M <- ( S - V[hi,1:n])/n
        R <- 2*M - V[hi,1:n]
        yR <- F(R)

        if (yR < Y[ho]) {
           if (Y[li] < yR) {
              V[hi,1:n] <- R
              Y[hi] <- yR
           } else {
              E <- 2*R - M
              yE <- F(E)
              if (yE < Y[li]) {
                 V[hi,1:n] <- E
                 Y[hi] <- yE
              } else {
                 V[hi,1:n] <- R
                 Y[hi] <- yR
              }
           }
        } else {
           if (yR < Y[hi]) {
              V[hi,1:n] <- R
              Y[hi] <- yR
           }
           C <- (V[hi,1:n] + M)/2
           yC <- F(C)
           C2 <- (M + R)/2
           yC2 <- F(C2)
           if (yC2 < yC) {
              C <- C2
              yC <- yC2
           }
           if (yC < Y[hi]) {
              V[hi,1:n] <- C
              Y[hi] <- yC
           } else {
              for (j in 1:(n+1)) {
                 if (j != lo) {
                    V[j,1:n] <- (V[j,1:n] + V[lo,1:n])/2
                    Z <- V[j,1:n]
                    Y[j] <- F(Z)
                 }
              }
           }
        }

        ho <- lo <- which.min(Y)
        li <- hi <- which.max(Y)
        for (j in 1:(n+1)) {
           if (j != lo && j != hi && Y[j] <= Y[li]) li <- j
           if (j != hi && j != lo && Y[j] >= Y[ho]) ho <- j
        }

        cnt <- cnt + 1
        if (show) {
            P <- rbind(P, V[lo, ])
            Q <- c(Q, Y[lo])
        }
    }

    snorm <- 0
    for (j in 1:(n+1)) {
       s <- abs(V[j] - V[lo])
       if (s >= snorm) snorm <- s
    }

    V0 <- V[lo, 1:n]
    y0 <- Y[lo]
    dV <- snorm
    dy <- abs(Y[hi] - Y[lo])

    if (show) {
        return(list(xmin = V0, fmin = y0/scale, niter = cnt,
                    dV = dV, dy = dy, P = P, Q = Q))
    } else {
        return(list(xmin = V0, fmin = y0/scale, niter = cnt))
    }
}
