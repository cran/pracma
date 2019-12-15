##
##  p o l y n o m i a l s . R  Polynomial Functions
##


# Generate a polynomial from its roots
Poly <- function(x) {
    if (is.null(x) || length(x) == 0) return(c(1))
    if (is.vector(x, mode="numeric") || is.vector(x, mode="complex")) {
        y <- x
    } else {
        if ((is.numeric(x) || is.complex(x))  && is.matrix(x)) {
            y <- eigen(x)$values
        } else {
            stop("Argument 'x' must be a vector or square matrix.")
        }
    }
    
    n <- length(y)
    p <- c(1, rep(0, n))
    for (i in 1:n) {
        p[2:(i+1)] <- p[2:(i+1)] - y[i] * p[1:i]
    }
    if (all(Im(p) == 0)) p <- Re(p)
    return(p)
}


# Add (and subtract) polynomials
polyadd <- function(p, q){
    if ( (!is.vector(p, mode="numeric") && !is.vector(p, mode="complex")) ||
         (!is.vector(q, mode="numeric") && !is.vector(q, mode="complex")) )
        stop("Arguments 'p' and 'q' must be real or complex vectors.")
    
    lp <- length(p)
    lq <- length(q)
    
    if (lp >= lq) {
        r <- p + c(numeric(lp-lq), q)
    } else {
        r <- q + c(numeric(lq-lp), p)
    }
    
    lr <- length(r)
    while (r[1] == 0 && lr > 1) {
        r <- r[2:lr]
        lr <- lr - 1
    }
    return(r)
}


# Multiply polynomials
polymul <- function(p, q){
    if ( (!is.vector(p, mode="numeric") && !is.vector(p, mode="complex")) ||
         (!is.vector(q, mode="numeric") && !is.vector(q, mode="complex")) )
        stop("Arguments 'p' and 'q' must be real or complex vectors.")
    
    n <- length(p); m <- length(q)
    if (n <= 1 || m <= 1) return(p*q)
    
    r <- rep(0, n+m-1)
    for (i in seq(along=q)) {
        r <- r + c(rep(0, i-1), p * q[i], rep(0, m-i))
    }
    
    while (r[1] == 0 && length(r) > 1)
        r <- r[2:length(r)]
    
    return(r)
}


# Take powers of polynomials
polypow <- function(p, n){
    if ( !is.vector(p, mode="numeric") && !is.vector(p, mode="complex") )
        stop("Arguments 'p' must be a real or complex vector.")
    if ( !is.numeric(n) || length(n) != 1 || floor(n) != ceiling(n) || n < 0 )
        stop("Argument 'n' must be a non-negative integer.")
    
    pp <- c(1)
    while (n > 0) {
        pp <- polymul(pp, p)
        n <- n - 1
    }
    return(pp)
}


# Divide polynomials using the 'deconv' function
polydiv <- function(p, q) {
    if (length(q) == 1)
        return(list(d = p/q, r = 0))
    qr <- deconv(p,q)
    d <- zapsmall(qr$q); r <- zapsmall(qr$r)
    return(list(d = d, r = r))
}


# 'Symbolic' derivative of a polynomial
polyder <- function(p, q) {
    if (!missing(q)) {
        if (length(q) == 0) return(0)
        if (!is.numeric(q) && !is.complex(q))
            stop("Arguments must be real or complex vectors or matrices.")
        m <- length(q)
        if (is.matrix(q)) q <- q[1:m]
    } else {
        q <- 1; m <- 1
    }
    
    if (length(p) == 0) return(0)
    if (!is.numeric(p) && !is.complex(p))
        stop("Argument 'p' must be a real or complex vector or matrix.")
    n <- length(p)
    if (is.matrix(p)) p <- p[1:n]
    
    # multiply polynomials p an q
    if (n*m <= 1) {
        return(0)
    } else {
        r <- rep(0, n+m-1)
        for (i in seq(along=q)) {
            r <- r + c(rep(0, i-1), p * q[i], rep(0, m-i))
        }
    }
    
    # case k > 1
    k <- length(r)
    r <- c((k-1):1) * r[1:(k-1)]
    
    while (r[1] == 0 && length(r) >= 2) {
        r <- r[2:length(r)]
    }
    return(r)
}


# 'Symbolic' antiderivative of a polynomial
polyint <- function(p, k=0) {
    if (length(p) == 0) return(c())
    if (!is.vector(p, mode="numeric") && !is.vector(p, mode="complex"))
        stop("Argument 'p' must be a real or complex vector.")
    if (!is.vector(k, mode="numeric") && !is.vector(k, mode="complex"))
        stop("Argument 'k' must be a real or complex vector")
    
    return( c(p / (length(p):1), k) )
}


# Transform a polynomial with another polynomial
polytrans <- function(p, q){
    if ( (!is.vector(p, mode="numeric") && !is.vector(p, mode="complex")) ||
         (!is.vector(q, mode="numeric") && !is.vector(q, mode="complex")) )
        stop("Arguments 'p' and 'q' must be real or complex vectors.")
    
    n <- length(p)
    if (length(p) == 1)
        return(p)
    
    pt <- 0
    for (i in 1:n) {
        pt <- polyadd(pt, p[i]*polypow(q, n-i))
    }
    
    return(pt)
}


# Print polynomial in normal form (highest powers first)
poly2str <- function(p, svar = "x", smul = "*",
                     d = options("digits")$digits) {
    if (length(p) == 0) return("")
    if (is.complex(p))
        stop("Printing of complex coefficients not yet implemented.")
    else if (!is.numeric(p))
        stop("Argument 'p' must be a numeric vector.")
    
    while (p[1] == 0 && length(p) > 1)
        p <- p[2:length(p)]
    if (length(p) == 1) return(as.character(p))
    
    s <- sign(p)
    p <- abs(p)
    
    p <- formatC(p, digits = d)
    p <- sub("^\\s+", "", p)
    
    n <- length(p) - 1
    S <- ""
    
    s1 <- if (s[1] == 1) "" else "-"
    S <- paste(s1, p[1], smul, svar, "^", n, sep = "")
    
    for (i in 2:(n+1)) {
        if (s[i] == 1) s1 <- " + "
        else if (s[i] == -1) s1 <- " - "
        else next
        
        if (n-i+1 > 1) {
            S <- paste(S, s1, p[i], smul, svar, "^", n-i+1, sep="")
        } else if (i == n) {
            S <- paste(S, s1, p[i], smul, svar, sep="")
        } else {
            S <- paste(S, s1, p[i], sep="")
        }
    }
    return(S)
}
