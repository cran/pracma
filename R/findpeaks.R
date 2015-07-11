findpeaks <- function(x,nups = 1, ndowns = nups, zero = "0", peakpat = NULL, 
                      # peakpat = "[+]{2,}[0]*[-]{2,}", 
                      minpeakheight = -Inf, minpeakdistance = 1,
                      threshold = 0, npeaks = 0, sortstr = FALSE)
{
    stopifnot(is.vector(x, mode="numeric"))
    if (! zero %in% c('0', '+', '-'))
        stop("Argument 'zero' can only be '0', '+', or '-'.")

    # transform x into a "+-+...-+-" character string
    xc <- paste(as.character(sign(diff(x))), collapse="")
    xc <- gsub("1", "+", gsub("-1", "-", xc))
    # transform '0' to zero
    if (zero != '0') xc <- gsub("0", zero, xc)

    # generate the peak pattern with no of ups and downs
    if (is.null(peakpat)) {
        peakpat <- sprintf("[+]{%d,}[-]{%d,}", nups, ndowns)
    }

    # generate and apply the peak pattern
    rc <- gregexpr(peakpat, xc)[[1]]
    if (rc[1] < 0) return(NULL)

    # get indices from regular expression parser
    x1 <- rc
    x2 <- rc + attr(rc, "match.length")
    attributes(x1) <- NULL
    attributes(x2) <- NULL

    # find index positions and maximum values
    n <- length(x1)
    xv <- xp <- numeric(n)
    for (i in 1:n) {
        xp[i] <- which.max(x[x1[i]:x2[i]]) + x1[i] - 1
        xv[i] <- x[xp[i]]
    }

    # eliminate peaks that are too low
    inds <- which(xv >= minpeakheight & xv - pmax(x[x1], x[x2]) >= threshold)

    # combine into a matrix format
    X <- cbind(xv[inds], xp[inds], x1[inds], x2[inds])

    # eliminate peaks that are near by
    if (minpeakdistance != 1)
        warning("Handling 'minpeakdistance' has not yet been implemented.")

    # Sort according to peak height
    if (sortstr) {
        sl <- sort.list(X[, 1], na.last = NA, decreasing = TRUE)
        X <- X[sl, , drop = FALSE]
    }

    # Return only the first 'npeaks' peaks
    if (npeaks > 0 && npeaks < nrow(X)) {
        X <- X[1:npeaks, , drop = FALSE]
    }

    if (length(X) == 0)    return(c())
    # else if (nrow(X) == 1) return(drop(X))
    else return(X)
}
