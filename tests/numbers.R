##
##  n u m b e rs .T  test suite
##


sigma <- pracma::sigma
tau <- pracma::tau
omega <- pracma::omega
Omega <- pracma::Omega

identical(sapply(1:16, sigma, k = 0),
          c(1, 2, 2, 3, 2, 4, 2, 4, 3, 4, 2, 6, 2, 4, 4, 5))
identical(sapply(1:16, sigma, k = 1),
          c(1, 3, 4, 7, 6, 12, 8, 15, 13, 18, 12, 28, 14, 24, 24, 31))
identical(sapply(1:16, sigma, proper = TRUE),
          c(0, 1, 1, 3, 1, 6, 1, 7, 4, 8, 1, 16, 1, 10, 9, 15))

all.equal(sapply(1:10, tau),
          c(1, -24, 252, -1472, 4830, -6048, -16744, 84480, -113643, -115920))

identical(sapply(1:16, omega),
          c(0, 1, 1, 1, 1, 2, 1, 1, 1, 2, 1, 2, 1, 2, 2, 1))
identical(sapply(1:16, Omega),
          c(0, 1, 1, 2, 1, 2, 1, 3, 2, 2, 1, 3, 1, 2, 2, 4))
