##
##  p o l y c n v . R  Test suite
##


polycnv <- pracma::polycnv

identical(polycnv(c(0.5), c(2, 4, 8)), c(1, 2, 4))
identical(polycnv(c(2.5, 1.5, 0.5), c(2)), c(5, 3, 1))
identical(polycnv(c(1, 1, 1), c(0, 1, 1, 1)), c(1, 2, 3, 2, 1))
identical(polycnv(c(1, 0, 0), c(0, 0, 1)), c(1, 0, 0))
