##
##  m t r a c e . R  Test suite
##

mtrace <- pracma::mtrace

identical(mtrace(1), 1)
identical(mtrace(matrix(1:9, 3, 3)), 15)
# Error: mtrace(matrix(1:12, 3, 4))
