##
##  v n o r m  Test suite
##


vnorm <- pracma::vnorm

identical(vnorm(c(3, 4)), 5)
identical(vnorm(c(1, 1, 1), p=2), sqrt(3))
identical(vnorm(1:10, p = 1), sum(1:10)+0.0)
identical(vnorm(1:10, p = 0), Inf)
identical(vnorm(1:10, p = Inf), max(1:10))
identical(vnorm(1:10, p = -Inf), min(1:10))
