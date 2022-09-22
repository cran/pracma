##
##  c i r c l e f i t . R  Fitting a Circle
##


circlefit <- function(xp, yp) {
    if (!is.vector(xp, mode="numeric") || !is.vector(yp, mode="numeric"))
        stop("Arguments 'xp' and 'yp' must be numeric vectors.")
    if (length(xp) != length(yp))
        stop("Vectors 'xp' and 'yp' must be of the same length.")

    n  <- length(xp)
    p <- qr.solve(cbind(xp, yp, 1), matrix(xp^2 + yp^2, ncol = 1))
    v <- c(p[1]/2, p[2]/2, sqrt((p[1]^2 + p[2]^2)/4 + p[3]))

    rms <- sqrt(sum((sqrt((xp-v[1])^2 + (yp-v[2])^2) - v[3])^2)/n)
    cat("RMS error:", rms, "\n")
    return(v)
}
