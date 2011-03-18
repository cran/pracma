##
##  m o d i n v . R  tests
##

modinv <- pracma::modinv

Modinv <- Vectorize(modinv, 'n')
all.equal(Modinv(1:10, 11), c(1, 6, 4, 3, 9, 2, 8, 7, 5, 10))
