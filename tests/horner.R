##
##  h or n e r . R  Test Suite
##


horner <- pracma::horner

p <- c(1, 0, 1)
x <- c(-2, -1, 0, 1, 2)
identical(horner(p, x)$y, x^2 + 1)
identical(horner(p, x)$dy, 2*x)

