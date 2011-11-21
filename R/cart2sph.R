##
##  c a r t 2 s p h . R  Coordinate Transformations
##


cart2sph <- function(xyz) {
    stopifnot(is.numeric(xyz), length(xyz) == 3)
    # Transform cartesian to spherical coordinates
    x <- xyz[1];  y <- xyz[2];  z <- xyz[3]

    hypotxy <- hypot(x, y)
    r       <- hypot(hypotxy, z)
    phi     <- atan2(z, hypotxy)
    theta   <- atan2(y, x)

    return(c(theta, phi, r))
}


sph2cart <- function(tpr) {
    stopifnot(is.numeric(tpr), length(tpr) == 3)
    # Transform spherical to cartesian coordinates
    theta <- tpr[1];  phi <- tpr[2];  r <- tpr[3] 

    z   <- r * sin(phi)
    tmp <- r * cos(phi)
    x   <- tmp * cos(theta)
    y   <- tmp * sin(theta)
    
    return(c(x, y, z))
}


cart2pol <- function(xyz) {
    stopifnot(is.numeric(xyz), length(xyz) == 2 || length(xyz) == 3)
    # Transform cartesian to cylindrical or polar coordinates
    x <- xyz[1];  y <- xyz[2]

    phi <- atan2(y, x)
    r   <- hypot(x, y)

    if (length(xyz) == 2) {
        return(c(phi, r))
    } else {
        z <- xyz[3]
        return(c(phi, r, z))
    }
}


pol2cart <- function(prz) {
    stopifnot(is.numeric(prz), length(prz) == 2 || length(prz) == 3)
    # Transform polar or cylindrical to cartesian coordinates
    phi <- prz[1];  r <- prz[2]

    x <- r * cos(phi)
    y <- r * sin(phi)

    if (length(prz) == 2) {
        return(c(x, y))
    } else {
        z <- prz[3]
        return(c(x, y, z))
    }
}
