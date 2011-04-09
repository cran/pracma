##
##  g c d . R  tests
##

extGCD <- pracma::extGCD
GCD <- pracma::GCD
LCM <- pracma::LCM

identical(extGCD(46368, 75025), c(1, 28657, -17711))
identical(GCD(46368, 75025), 1)
identical(LCM(46368, 75025), 46368 * 75025)
