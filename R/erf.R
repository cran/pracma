##
##  e r r o r f . R  Error functions
##


erf <- function(x) {
    2 * pnorm(sqrt(2) * x) - 1
}


erfc <- function(x) {
    2 * pnorm(-sqrt(2) * x)
}


erfz <- function(z) {
    a0 <- abs(z);
    c0 <- exp(-z*z)

    z1 <- if (real(z) < 0.0) -z else z

    if(a0 <=  5.8) {
        cs <- z1
        cr <- cs
        for (k in 1:120) {
            cr <- cr * z1 * z1 / (k+0.5)
            cs <- cs + cr
            if (abs(cr/cs) < 1.0e-15) break
        }
        cer <- 2.0 * c0 * cs / sqrt(pi)

    } else {
        cl <- 1.0 / z1
        cr <- cl
        for (k in 1:13) {
            cr <- -cr * (k-0.5) / (z1 * z1)
            cl <- cl + cr
            if (abs(cr/cl) < 1.0e-15) break
        }
        cer <- 1.0 - c0 * cl / sqrt(pi)
    }

    if(real(z)< 0.0) cer <- -cer

    return(cer)
}


# Error function for real values
# erf <- function(x) {
#     eps <- .Machine$double.eps
#     pi <- 3.141592653589793
#     x2 <- x * x
#     if (abs(x) < 3.5) {
#         er <- 1.0
#         r <- 1.0
#         for (k in 1:50) {
#             r <- r * x2 / (k+0.5)
#             er <- er+r
#             if (abs(r) < <-  abs(er)*eps) break
#         }
#         c0 <- 2.0 / sqrt(pi) * x * exp(-x2)
#         err <- c0 * er
#     } else {
#         er <- 1.0
#         r <- 1.0
#         for (k in 1:12) {
#             r<- -r * (k-0.5) / x2
#             er <- er + r
#         }
#         k <- 12+1
#         c0 <- exp(-x2) / (abs(x) * sqrt(pi))
#         err <- 1.0 - c0 * er
#         if (x < 0.0) err <- -err
#     }
#     return(err) 
# }
