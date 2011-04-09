##
##  m o d e . R
##


mode <- pracma::mode

x <- c(1:100, rep(5,3), rep(27,5), rep(71,4), rep(89,2), rep(100, 5))
identical(mode(x), 27)

x <- as.factor(x)
identical(mode(x), "27")

# order of complex values?
# x <- c(1+1i, 1i, -1, -1i, 1-1i, 1)
# identical(mode(x), 1i)
