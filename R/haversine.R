##
##  h a v e r s i n e . R  Haversine Formula
##


haversine <- function(loc1, loc2, R = 6371.0) {
    if (is.character(loc1)) {
        locs <- strsplit(loc1, ',')[[1]]
        lat1 <- locs[1]; lon1 <- locs[2]
        lat1 <- .check_coords(lat1) * pi / 180
        lon1 <- .check_coords(lon1) * pi / 180
    } else if (is.numeric(loc1)) {
        if (length(loc1) != 2)
            stop("Coordinate input not in correct format.")
        lat1 <- loc1[1]; lon1 <- loc1[2]
        if (abs(lat1) > 90 || abs(lon1) > 180)
            stop("Coordinate input not in correct format.")
        lat1 <- lat1 * pi /180; lon1 <- lon1 * pi / 180
    } else {
        stop("Location must be given as string 'lat lon'.")
    }
    if (is.character(loc2)) {
        locs <- strsplit(loc2, ',')[[1]]
        lat2 <- locs[1]; lon2 <- locs[2]
        lat2 <- .check_coords(lat2) * pi / 180
        lon2 <- .check_coords(lon2) * pi / 180
    } else if (is.numeric(loc2)) {
        if (length(loc2) != 2)
            stop("Coordinate input not in correct format.")
        lat2 <- loc2[1]; lon2 <- loc2[2]
        if (abs(lat2) > 90 || abs(lon2) > 180)
            stop("Coordinate input not in correct format.")
        lat2 <- lat2 * pi /180; lon2 <- lon2 * pi / 180
    } else {
        stop("Location must be given as string 'lat, lon'.")
    }
    # R <- 6371.0  # average earth radius [km]
    dlat <- lat2 - lat1
    dlon <- lon2 - lon1

    # Haversine formula
    a <- sin(dlat/2)^2 + 
         cos(lat1) * cos(lat2) * sin(dlon/2)^2;
    c <- 2 * atan2(sqrt(a), sqrt(1-a))
    return(R * c)
}


.check_coords <- function(s) {
    m <- gregexpr("^\\s*(\\d+)\\s+(\\d+)\\s+(\\d*)\\s*(\\w?)\\s*$",
                  s, perl=TRUE)
    m <- m[[1]]
    if (m[1] != 1) stop("Coordinate input not in correct format.")

    strt <- attr(m, "capture.start")
    lngt <- attr(m, "capture.length")
    c1 <- as.numeric( substr(s, strt[1], strt[1]+lngt[1]-1) )
    c2 <- as.numeric( substr(s, strt[2], strt[2]+lngt[2]-1) )
    c3 <- as.numeric( substr(s, strt[3], strt[3]+lngt[3]-1) )

    c0 <- c1 + c2/60 + c3/3600
    if (c1 > 180 || c2 >= 60 || c3 >= 60)
        stop("Coordinate input not in correct format.")

    c4 <- substr(s, strt[4], strt[4]+lngt[4]-1)
    if (c4 == 'S' || c4 == 'W') {
        c0 <- -1 * c0
    } else if (c4 != 'N' && c4 != 'E') {
        stop("Coordinate input not in correct format.")
    }
    return(c0)
}
