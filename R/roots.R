###
### ROOTS.R  Matlab ROOTS Function
###

roots <- function(p) {
    if (is.null(p) || length(p) == 0) return(matrix(0, nrow=0, ncol=0))
    if ( !is.vector(p, mode="numeric") &&
         !is.vector(p, mode="complex") )
        stop("Argument p must be a vector of real or complex numbers.")
    if (length(p) == 1) return(matrix(0, nrow=0, ncol=0))

    # Find non-zero entries in p
    inz <- which(p != 0)
    nnz <- length(inz)
    if (nnz == 0) return(c())

    # Strip leading and trailing zeros, but remember the trailing zeros
    q <- p[inz[1]:inz[nnz]]
    r <- rep(0, length(p) - inz[nnz])

    A <- compan(q)
    return(c(r, eig(A)))
}


# Greatest common factor/divisor of polynomials
polygcf <- function(p, q, tol=1e-12) {
    if ( !is.vector(p, mode="numeric") && !is.vector(p, mode="complex") )
        stop("Arguments 'p' must be a real or complex vector.")
    if ( !is.vector(q, mode="numeric") && !is.vector(q, mode="complex") )
        stop("Arguments 'q' must be a real or complex vector.")
    
    np <- Norm(p)
    pd <- polydiv(p,q)
    a <- pd$d; r0 <- pd$r
    if (Norm(r0) > np*tol) {
        pd <- polydiv(q,r0)
        a <- pd$d; r1 <- pd$r
        if (Norm(r1) > np*tol) {
            rn <- 1
            while (Norm(rn) > np*tol) {
                pd <- polydiv(r0,r1)
                a <- pd$d; rn <- pd$r
                r0 <- r1
                r1 <- rn
            }
            g <- r0
        } else {
            g <- r0
        }
    } else {
        g <- q
    }
    # g <- g / g[1]
    return(g)
}


# Determine multiplicity of polynomial roots
rootsmult <- function(p, r, tol=1e-12) {
    np <- Norm(p)
    v <- abs(polyval(p, r))
    if (v > np*tol) {
        warning("Value 'r' is not root of polynomial 'p'.")
        return(0)
    } else {
        n <- 0
        while (v < np*tol) {
            p <- polyder(p)
            v <- abs(polyval(p,r))
            n <- n+1
        }
        return(n)
    }
}


# Find the multiplicity of all roots of a polynomial
polyroots <- function(p, ntol = 1e-04, ztol = 1e-08) {
    stopifnot(is.numeric(p))
    
    # Multiplicity of 0 as root
    wp <- which(p != 0)
    minp <- min(wp); maxp <- max(wp)
    mz <- length(p) - maxp
    p0 <- p[minp:maxp]
    zp <- zm <- c()
    if (mz > 0)  {zp <- c(zp, 0); zm <- c(zm, mz)}
    if (length(p0) < 2) return(data.frame(root = zp, mult = zm))
    
    s <- abs(p0[length(p0)] / p0[1])
    if (s < 1)  p0 <- p0[length(p0):1]
    
    q0 <- polyder(p0)
    g1 <- p0 / p0[1]
    g2 <- q0[1:max(which(q0 != 0))] / q0[1]
    
    for (k in 3:(2*length(p0))) {
        l12 <- length(g1) - length(g2); l21 <- -l12
        g3 <- c(g2, zeros(1, l12)) - c(g1, zeros(1, l21))
        wh <- which(abs(g3) > ztol)
        g3 <- if (isempty(wh)) c(0) else g3[min(wh):max(wh)]
        ren <- Norm(g3, Inf) / Norm(g2, Inf)
        if (ren < ntol) break
        if(l12 >= 0)  g1 <- g2
        g2 <- g3 / g3[1]
    }
    
    g0 <- g1
    u0 <- deconv(p0, g0)$q
    v0 <- deconv(q0, g0)$q
    w0 <- polyder(u0)
    z0 <- roots(u0)
    m0 <- polyval(v0, z0) / polyval(w0, z0)
    if (s < 1) z0 <- z0^-1
    
    zp <- c(zp, z0); zm <- c(zm, round(abs(m0)))
    return(data.frame(root = zp, mult = zm))
}
