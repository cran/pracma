###
### poly.R  +++ Test suite +++
###


test.poly <- function(input, expected) {
   output <- do.call(getFromNamespace("poly", "pracma"), input)
   identical(output, expected)
}

poly.expected.empty  <- 1
poly.expected.1 <- c(1, -6, 11, -6)
#poly.expected.2 <- error
poly.expected.3 <- c(1, 0, 0, 0, -1)
poly.expected.4 <- c(1, -10, 35, -50, 24)
poly.expected.5 <- c(1, -4, 6, -4, 1)
poly.expected.6 <- c(1, -5)

test.poly(list(x=c()), poly.expected.empty)
test.poly(list(x=c(1,2,3)), poly.expected.1)
#test.poly(list(x=matrix(1:6, 2, 3)), poly.expected.2)
test.poly(list(x=c(1,-1,1i,-1i)), poly.expected.3)
test.poly(list(x=c(1,2,3,4)), poly.expected.4)
test.poly(list(x=diag(4)), poly.expected.5)
test.poly(list(x=5), poly.expected.6)
