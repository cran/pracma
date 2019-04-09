##
##  q p s p e c i a l . R
##


qpspecial <- function(G, x, maxit = 100) {
    stopifnot(is.numeric(G), is.matrix(G))

    m <- nrow(G); n <- ncol(G)
    if (m*n <= 0) {
        warning("qpspecial: Matrix 'G' is empty; nothing can be done.")
        return(list(x = c(), d = c(), q = Inf, niter = 0, info = 2))
    }
    maxit <- max(floor(maxit), 5)

    e <- matrix(1, n, 1)
    if (missing(x)) {
        x <- e
    } else {
        x <- as.matrix(c(x))
        nx <- length(x)
        if (any(x < 0) || nx != n)
            x <- e
    }

    idx <- seq(1, (n*n), by = n+1)
    Q <- t(G) %*% G
    z <- x
    y <- 0
    eta <- 0.9995
    delta <- 3
    mu0 <- sum(x*z)/n
  
    tolmu <- 1e-5
    tolrs <- 1e-5
    kmu <- tolmu * mu0
  
    nQ <- norm(Q, "I") + 2
    krs <- tolrs * nQ

    ap <- 0; ad <- 0

    for (k in 1:maxit) {

        r1 <- -Q %*% x + e*y + z
        r2 <- -1 + sum(x)
        r3 <- -x * z
        rs <- norm(rbind(r1, r2), "I")
        mu <- -sum(r3)/n
    
        if (mu < kmu) {
            if (rs < krs) {
                niter <- k-1; info <- 0
                break
            }
        }

        zdx <- z / x
        QD <- Q
        QD[idx] <- QD[idx] + zdx
        C <- chol(QD)
        KT <- solve(t(C), e)
        M <- sum(KT * KT)

        r4 <- r1 + r3/x
        r5 <- sum(KT * solve(t(C), r4))
        r6 <- r2 + r5
        dy <- -r6/M
        r7 <- r4 + e*dy
        dx <- solve(C, solve(t(C), r7))
        dz <- (r3 - z*dx)/x

        p <- -x / dx
        p0 <- p[p > 0]
        if (length(p0) > 0) { ap <- min(p0, 1)
        } else              { ap <- 1 }
        p <- -z / dz
        p0 <- p[p > 0]
        if (length(p0) > 0) { ad <- min(p0, 1)
        } else              { ad <- 1 }

        mauff <- sum((x + ap*dx) * (z + ad*dz)) / n
        sig <- (mauff/mu)^delta
    
        r3 <- r3 + sig*mu
        r3 <- r3 - dx*dz
        r4 <- r1 + r3/x
        r5 <- sum(KT * solve(t(C), r4))
        r6 <- r2 + r5
        dy <- -r6/M
        r7 <- r4 + e*dy
        dx <- solve(C, solve(t(C), r7))
        dz <- (r3 - z*dx)/x

        p <- -x / dx
        p0 <- p[p > 0]
        if (length(p0) > 0) { ap <- min(p0, 1)
        } else              { ap <- 1 }
        p <- -z/dz
        p0 <- p[p > 0]
        if (length(p0) > 0) { ad <- min(p0, 1)
        } else              { ad <- 1 }

        x <- x + eta * ap * dx
        y <- y + eta * ad * dy
        z <- z + eta * ad * dz
    }

    if (k == maxit) info <- 1
    x <- pmax(x,0)
    x <- x/sum(x)
    d <- G %*% x
    q <- sum(d * d)
    list(x = x, d = d, q = q, niter = k, info = info)
}


qpsolve <- function(d, A, b, meq = 0, tol = 1e-07){
    sol <- dvec <- d
    bvec <- b
    imeq <- seq_len(meq)
    
    Nmat <- NULL
    wvec <- NULL
    active <- NULL
    niter <- 0
    repeat{
        niter <- niter + 1
        viol <-  crossprod(A, sol) - bvec
        viol1 <- viol / pmax(1,abs(bvec))
        iim <- viol1[imeq] >= tol
        if( any(iim) ){
            iim <- which(iim)
            viol[iim] <- -viol[iim]
            bvec[iim] <- -bvec[iim]
            A[,iim] <- -A[,iim]
        }
        ii <- which.min(viol1)[1]
        if( viol1[ii] > -tol) break
        if(ii %in% active) stop("Error in projection")
        wvec <- c(wvec, 0)
        active <- c(active, ii)
        npvec <- A[,ii]
        if( !is.null(Nmat) ){
            rvec <- solve(qr(Nmat, LAPACK=TRUE), npvec)
            dvec <- npvec - c(Nmat %*% rvec)
        }else{
            dvec <- npvec
            rvec <- NULL
        }
        jj <- rvec > 0
        jj[1] <- FALSE
        tmp <- wvec[jj]/rvec[jj]
        t1 <- suppressWarnings(min(tmp))
        t2 <- -viol[ii]/crossprod(npvec, dvec)
        t <- min(c(t1, t2))
        if( !is.finite(t) || t < 0 || t1 <= t2 ) stop("Error in projection")
        sol <- sol + t * dvec
        wvec <- wvec - t * c(rvec, -1)
        Nmat <- cbind(Nmat, npvec)
    }
    sol <- c(sol)
    val <- 0.5 * sum(sol * sol) - sum(d * sol)
    return(list(sol = sol, val = val, niter = niter))
}
