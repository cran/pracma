##
##  m t r a c e . R  Test suite
##

mtrace <- pracma::mtrace

identical(mtrace(1), 1)
identical(mtrace(matrix(c(1,2,3,4,5,6,7,8,9), 3, 3)), 15)
# Error: mtrace(matrix(1:12, 3, 4))
