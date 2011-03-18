##
##  r e p m a t . R  Test Suite
##

repmat <- pracma::repmat
reshape <- pracma::reshape

v <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
a <- matrix(v, 3, 4)
identical(reshape(a, 4, 3), matrix(v, 4, 3))

identical(repmat(matrix(1:4, 2, 2), 3),
          matrix(rep(c(rep(c(1,2), 3), rep(c(3, 4), 3)), 3), nrow=6, ncol=6))
