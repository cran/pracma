##
##  s i m p s o n . R  Simpson Integration
##


simpson <- function(f, a, b, n = 100, ...) {
    # Implementation of Simpson's 1/3 rule for numerical integration
    #
    # Args:
    #   f: The function to integrate (must be vectorized)
    #   a: Lower limit of integration
    #   b: Upper limit of integration
    #   n: Number of subintervals (must be even)
    #
    # Returns:
    #   The approximate integral of f from a to b
    
    if (n %% 2 != 0) {
        stop("n must be even for Simpson's rule")
    }
    
    h <- (b - a) / n
    x <- seq(a, b, length.out = n + 1)
    y <- f(x)
    
    # Simpson's rule weights: 1, 4, 2, 4, 2, ..., 4, 1
    weights <- rep(c(4, 2), times = n/2)
    weights <- c(1, weights)
    weights[length(weights)] <- 1
    
    integral <- (h / 3) * sum(weights * y)
    
    return(integral)
}


simpson2d <- function(f, xa, xb, ya, yb, nx = 128, ny = 128, ...) {
    stopifnot(is.numeric(xa), length(xa) == 1, is.numeric(xb), length(xb) == 1,
              is.numeric(ya), length(ya) == 1, is.numeric(yb), length(yb) == 1)
    
    fun <- match.fun(f)
    f <- function(x, y) fun(x, y, ...)
    
    if (nx %% 2 != 0) nx <- nx + 1
    if (ny %% 2 != 0) ny <- ny + 1
    
    # Grid and grid vectors
    hx <- (xb - xa) / nx
    hy <- (yb - ya) / ny
    xg <- seq(xa, xb, by = hx)
    yg <- seq(ya, yb, by = hy)
    
    # Interchange meshgrid
    mgrid <- meshgrid(yg, xg)
    X <- mgrid$Y
    Y <- mgrid$X
    
    F <- f(X, Y)
    
    # Contributions from the corner points
    s1 <- F[1, 1] + F[1, ny+1] + F[nx+1, 1] + F[nx+1, ny+1]
    
    # Contributions from other edge points
    ixo <- seq(2, nx, by = 2); ixe <- seq(3, nx-1, by = 2)
    iyo <- seq(2, ny, by = 2); iye <- seq(3, ny-1, by = 2)
    s2 <- 2 * ( sum(F[1, iye]) + sum(F[nx+1, iye]) + sum(F[ixe, 1]) + sum(F[ixe, ny+1]) );
    s3 <- 4 * ( sum(F[1, iyo]) + sum(F[nx+1, iyo]) + sum(F[ixo, 1]) + sum(F[ixo, ny+1]) );
    
    # Contributions from interior points
    s4 <- 16 * sum( sum( F[ixo,iyo] ) ) + 4 * sum( sum( F[ixe,iye] ) );
    s5 <-  8 * sum( sum( F[ixe,iyo] ) ) + 8 * sum( sum( F[ixo,iye] ) );
    
    S <- hx * hy * (s1 + s2 + s3 + s4 + s5) / 9.0
    return(S)
}
