##
##  p r i m r o o t . R  Test suite
##


modpower <- pracma::modpower
modorder <- pracma::modorder
primroot <- pracma::primroot

modpower
identical(modpower(3, 0, 4), 1)
identical(modpower(3, 1, 1), 0)
identical(modpower(3, 100, 17), 13)
identical(modpower(0, 3, 4), 0)

identical(primroot(71), 7L)
identical(modorder(7, 71), 70)
