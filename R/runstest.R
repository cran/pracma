##
##  r u n s t e s t . R  Runs Test
##


runs.test <- function(x) {
    if (is.null(x) || !is.vector(x))
        stop("Argument 'x' must be a (numeric or non-numeric) vector.")

    rtab <- table(x)
    if (length(rtab) != 2)
        stop("Argument 'x' shall have exactly two different elements.")

    nms <- names(rtab)
    rtab <- unname(rtab)
    n <- rtab[1]; m <- rtab[2]
    N <- n + m

    flips <- ifelse(x == nms[1], 0, 1)
    dflips <- c(1, abs(diff(flips)))
    R <- sum(dflips!=0)         # counts no. of runs

    mu <- 2*n*m/N + 1           # mean, expected no. or runs
    vr <- (mu-1)*(mu-2)/(N-1)   # variance, standard deviation
    sigma <- sqrt(vr)

    if (N > 50) {               # test statistics
        Z  <- (R - mu) / sigma
    } else if ((R - mu) < 0){
        Z = (R - mu + 0.5) / sigma
    } else {
        Z = (R - mu - 0.5) / sigma
    }

    pval <- 2*(1-pnorm(abs(Z))) # two-sided p-value
    return(list(Z = Z, p.value = pval))
}
