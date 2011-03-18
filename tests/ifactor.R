###
### IFACTOR.R  +++ Test suite +++
###


test.ifactor <- function(input, expected) {
    output <- do.call(getFromNamespace("ifactor", "pracma"), input)
    identical(output, expected)
}

ifactor.expected.n2  <- 2
ifactor.expected.n3  <- 3
ifactor.expected.prm <- 999983
ifactor.expected.pr2 <- c(9999889, 9999901)
ifactor.expected.prp <- c(65003, 65003)
ifactor.expected.nn  <- c(2, 2, 2, 2, 2, 3, 3, 3, 3, 5, 5, 5, 7, 7, 11)
ifactor.expected.nm  <- c(99989, 99991, 100003)
ifactor.expected.n32 <- c(3, 5, 17, 257, 65537)

test.ifactor(list(n=2), ifactor.expected.n2)
test.ifactor(list(n=3), ifactor.expected.n3)
test.ifactor(list(n=999983), ifactor.expected.prm)
test.ifactor(list(n=9999889*9999901), ifactor.expected.pr2)
test.ifactor(list(n=4225390009), ifactor.expected.prp)
test.ifactor(list(n=2^5 * 3^4 * 5^3 * 7^2 * 11), ifactor.expected.nn)
test.ifactor(list(n=99989*99991*100003), ifactor.expected.nm)
test.ifactor(list(n=2^32-1), ifactor.expected.n32)
