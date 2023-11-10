anms <- function(fn, x0, ...,
                 tol = 1e-10, maxfeval = NULL) {
    stopifnot(is.numeric(x0) && length(x0) > 1)
    d <- length(x0); d1 <- d + 1
    fun <- match.fun(fn)
    fn <- function(x) fun(x, ...)

    if (is.null(maxfeval)) maxfeval <- 50 * d^2

    # set up adaptive parameters
    alpha <- 1; beta <- 1 + 2/d
    gamma <- 0.75 - 0.5/d; delta <- 1 - 1/d

    # large initial simplex is used
    scalefactor <- min(max(max(abs(x0)), 1), 10)

    # construct the initial simplex
    D0 <- rbind(eye(d),
                (1-sqrt(d1)) / d * ones(1,d))
    X <- matrix(0, nrow = d1, ncol = d)
    FX <- numeric(d1)
    for (i in 1:d1) {
        X[i, ] <- x0 + scalefactor * D0[i, ]
        FX[i] <- fn(X[i, ])
    }
    o <-order(FX, decreasing = FALSE)
    FX <- FX[o]; X <- X[o, ]

    # main iteration
    ct <- d1
    while (max(abs(X[2:d1, ] - X[1:d, ])) >= scalefactor*tol) {
        if (ct > maxfeval) break
        M <- apply(X[1:d, ], 2, mean)
        xref <- (1+alpha) * M - alpha * X[d1, ]
        Fref <- fn(xref)
        ct <- ct + 1
        if (Fref < FX[1]) {
            # expansion
            xexp <- (1+alpha*beta) * M - alpha*beta*X[d1, ]
            Fexp <- fn(xexp)
            ct <- ct+1
            if (Fexp < Fref) {
                X[d1, ] <- xexp
                FX[d1] <- Fexp
            } else {
                X[d1, ] <- xref
                FX[d1] <- Fref
            }
        } else {
            if (Fref < FX[d]) {
                # accept reflection point
                X[d1, ] <- xref
                FX[d1] <- Fref
            } else {
                if (Fref < FX[d1]) {
                    # Outside contraction
                    xoc <- (1+alpha*gamma) * M - alpha*gamma*X[d1, ]
                    Foc <- fn(xoc)
                    ct <- ct+1;
                    if (Foc <= Fref) {
                        X[d1, ] <- xoc
                        FX[d1] <- Foc
                    } else {
                        # shrink
                        for (i in 2:d1) {
                            X[i, ] <- X[1, ] + delta*(X[i, ] - X[1, ])
                            FX[i] <- fn(X[i, ])
                        }
                        ct=ct+d
                    }
                } else {
                    # inside contraction
                    xic <- (1-gamma) * M + gamma*X[d1, ]
                    Fic <- fn(xic)
                    ct <- ct+1
                    if (Fic < FX[d1]) {
                        X[d1, ] <- xic
                        FX[d1] <- Fic
                    } else {
                        # shrink
                        for (i in 2:d1) {
                            X[i, ] <- X[1, ] + delta*(X[i, ] - X[1, ])
                            FX[i] <- fn(X[i, ])
                        }
                        ct <- ct+d
                    }
                }
            }
        }
        o <-order(FX, decreasing = FALSE)
        FX <- FX[o]; X <- X[o, ]
    }
    xmin <- X[1, ]
    fmin <- FX[1]

    return(list(xmin = xmin, fmin = fmin, nfeval = ct))
}