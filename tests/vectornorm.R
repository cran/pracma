##
##  v e c t o r n o r m  Test suite
##


vectornorm <- pracma::vectornorm

identical(vectornorm(c(3, 4)), 5)
identical(vectornorm(c(1, 1, 1), p=2), sqrt(3))
identical(vectornorm(1:10, p = 1), sum(1:10)+0.0)
identical(vectornorm(1:10, p = 0), Inf)
identical(vectornorm(1:10, p = Inf), max(1:10))
identical(vectornorm(1:10, p = -Inf), min(1:10))
