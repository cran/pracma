##
##  t e s t f u n c t i o n s . R  Test Functions
##


rastrigin <- function(x) {
    n <- length(x)
    10*n + sum(x^2 - 10*cos(2*pi*x))
}

rosenbrock <- function(x) {
    n <- length(x)
    x1 <- x[2:n]
    x2 <- x[1:(n-1)]
    sum(100*(x1-x2^2)^2 + (1-x2)^2)
}
