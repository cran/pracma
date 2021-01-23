##
##  g a m m a i n c . R  Incomplete Gamma Function
##


gammainc <- function(x, a) {
    if (!is.numeric(a) || !is.numeric(x))
        stop("All arguments must be real numbers.")
    if (length(a) > 1 || length(x) > 1)
        stop("Arguments must be of length 1; function is not vectorized.")
    if (a < 0)
        stop("Argument 'a' must be real and nonnegative.")
    if (x == 0 && a == 0)
        return(c(lowinc = 0.0, uppinc = Inf, reginc = 0.0))
    if (x == 0)
        return(c(lowinc = 0.0, uppinc = gamma(a), reginc = 0.0))

    if (x > 0)  xam <- -x + a*log(x)
    else        xam <- -x + a*log(x + 0i)
    if (abs(xam) > 700.0 || abs(a) > 170.0) {
        warning("Arguments 'x' and/or 'a' are too large.")
        return(NA)
    }

    # Computation of the incomplete gamma function
    gin <- gim <- gip <- 0

    if (x == 0.0) {
        ga <- gamma(a)
        gim <- ga
        gip <- 0.0
    } else if (x <= 1.0 + a) {
        s <- 1/a
        r <- s
        for  (k in 1:60) {
            r <- r * x/(a+k);
            s <- s+r;
            if (abs(r/s) < 1e-15) break
        }
        gin <- exp(xam) * s
        ga <- gamma(a)
        gip <- gin/ga
        gim <- ga - gin
    } else if (x > 1.0 + a) {
        t0 <- 0
        for  (k in 60:1) {
            t0 <- (k-a)/(1 + k/(x+t0))
        }
        gim <- exp(xam)/(x+t0)
        ga <- gamma(a)
        gin <- ga - gim
        gip <- 1 - gim/ga
    }
    return(c(lowinc = Re(gin), uppinc = Re(gim), reginc = Re(gip)))
}


incgam <- function(x, a) {
    if (!is.numeric(a) || !is.numeric(x)) 
        stop("All arguments must be real numbers.")
    if (length(a) > 1 || length(x) > 1) 
        stop("Arguments must be of length 1; function is not vectorized.")
    
    if (x > 0) {
        if (a > 0) {
            g_gamma <- gamma(a)
            g_upper <- g_gamma * pgamma(x, a, 1, lower.tail = FALSE)
            # g_regul <- pgamma(x, a, 1, lower = TRUE)
            # g_lower <- g_gamma * g_regul
        } else if (a == 0) {
            g_upper <- pracma::expint_E1(x)
        } else if (a < 0 && a >= -1) {
            g_upper <- -1 * x^a*exp(-x)/a + incgam(x, a+1)/a
        } else { # (a < 0)
            stop("Not yet implemented: use recursion -- see help")
        }
        
    } else if (x == 0) {
        g_upper <- gamma(a)
    } else { # (x < 0)
        stop("Not implemented: Result for 'x<0' will be complex.")
    }
    
    # g_lower <- gamma(a) - g_upper
    # g_regul <- 1 - g_upper / gamma(a)
    return(g_upper)
}

