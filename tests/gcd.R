##
##  g c d . R  tests
##

extgcd <- pracma::extgcd
gcd <- pracma::gcd
lcm <- pracma::lcm

identical(extgcd(46368, 75025), c(1, 28657, -17711))
identical(gcd(46368, 75025), 1)
identical(lcm(46368, 75025), 46368 * 75025)
