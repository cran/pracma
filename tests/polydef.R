###
### polydef.R  +++ Test suite +++
###


test.polydef <- function(input, expected) {
   output <- do.call(getFromNamespace("polydef", "pracma"), input)
   identical(output, expected)
}

polydef.expected.empty  <- 1
polydef.expected.1 <- c(1, -6, 11, -6)
#polydef.expected.2 <- error
polydef.expected.3 <- c(1, 0, 0, 0, -1)
polydef.expected.4 <- c(1, -10, 35, -50, 24)
polydef.expected.5 <- c(1, -4, 6, -4, 1)
polydef.expected.6 <- c(1, -5)

test.polydef(list(x=c()), polydef.expected.empty)
test.polydef(list(x=c(1,2,3)), polydef.expected.1)
#test.polydef(list(x=matrix(1:6, 2, 3)), polydef.expected.2)
test.polydef(list(x=c(1,-1,1i,-1i)), polydef.expected.3)
test.polydef(list(x=c(1,2,3,4)), polydef.expected.4)
test.polydef(list(x=diag(4)), polydef.expected.5)
test.polydef(list(x=5), polydef.expected.6)
