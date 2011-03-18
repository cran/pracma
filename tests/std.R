##
##  s t d . r  tests
##

std <- pracma::std

all.equal(std(1:10), 3.0277, tolerance=0.0001)
all.equal(std(1:10, flag=0), 3.0277, tolerance=0.0001)
all.equal(std(1:10, flag=1), 2.8723, tolerance=0.0001)
