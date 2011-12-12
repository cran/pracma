pltcross <- function(x, y, cx = 0, cy = 0, clen = 0.1,
                  cent = FALSE, color = "darkgrey", ...) {
    stopifnot(is.numeric(x),  is.numeric(y),
              is.numeric(cx), is.numeric(cy), is.numeric(clen))
    if (length(cx) != length(cy))
        stop("Length of arguments 'cx', 'cy' must be the same.")
    if (length(clen) < length(cx)) {
        m <- length(cx); n <- length(clen)
        clen <- c(rep(clen, m %/% n), clen[1:(m %% n)])
    }
    if (length(color) < length(cx)) {
        m <- length(cx); n <- length(color)
        color <- c(rep(color, m %/% n), color[1:(m %% n)])
    }

    if (any(clen <= 0 || clen > 1)) {
        clen <- ifelse(clen > 0 && clen <= 1, clen, 1)
        warning("Proportional length 'clen' should be between 0 and 1.")
    }

    clx <- (max(x) - min(x)) * clen / 2
    cly <- (max(y) - min(y)) * clen / 2

    for (i in 1:length(cx)) {
        lines(c(cx[i]-clx[i], cx[i]+clx[i]), c(cy[i], cy[i]), col = color[i], ...)
        lines(c(cx[i], cx[i]), c(cy[i]-cly[i], cy[i]+cly[i]), col = color[i], ...)
    }
    if (cent)
        points(cx, cx, col = color)

    invisible(NULL)
}
