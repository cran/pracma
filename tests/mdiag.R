##
##  m t r a c e . R  Test suite
##

mdiag <- pracma::mdiag

identical(mdiag(matrix(1:12,3,4),  1), c(4,8,12))
identical(mdiag(matrix(1:12,3,4), -1), c(2,6))
identical(mdiag(mdiag(c(1,5,9)), 0), c(1,5,9))
