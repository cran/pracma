##
##  c e i l . R  tests
##

ceil <- pracma::ceil
fix <- pracma::fix

identical(ceil(0), 0)
identical(ceil(-1), -1)
identical(ceil(-1.5), -1)
identical(ceil(1), 1)
identical(ceil(1.5), 2)

identical(fix(0), 0)
identical(fix(-1), -1)
identical(fix(-1.5), -1)
identical(fix(1), 1)
identical(fix(1.5), 1)
