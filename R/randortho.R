##
##  r a n d o r t h o . R  Generate Random Orthogonal Matrix
##


randortho <- function(n, type = c("orthonormal", "unitary")) {
    stopifnot(is.numeric(n), length(n) == 1,
              floor(n) == ceiling(n), n >= 1)
    if (n == 1) 
        return(matrix(1, 1, 1))
    
    type <- match.arg(type)
    if (type == "orthonormal") {
        z <- randn(n, n) / sqrt(2.0)
    } else {
        z <- (randn(n, n) + 1i * randn(n, n)) / sqrt(2.0)
    }
    
    # QR decomposition for real or complex matrices
    Z <- qr(z)
    q <- qr.Q(Z); r <- qr.R(Z)
    
    d <- diag(r)
    ph <- d/abs(d)
    q %*% diag(ph)
}
