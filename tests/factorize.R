###
### f a c t o r i z e . R  +++ Test suite +++
###


test.factorize <- function(input, expected) {
    output <- do.call(getFromNamespace("factorize", "pracma"), input)
    identical(output, expected)
}

factorize.expected.n2  <- 2
factorize.expected.n3  <- 3
# factorize.expected.prm <- 999983
# factorize.expected.pr2 <- c(9999889, 9999901)
# factorize.expected.prp <- c(65003, 65003)
# factorize.expected.nn  <- c(2, 2, 2, 2, 2, 3, 3, 3, 3, 5, 5, 5, 7, 7, 11)
# factorize.expected.nm  <- c(99989, 99991, 100003)
# factorize.expected.n32 <- c(3, 5, 17, 257, 65537)

test.factorize(list(n=2), factorize.expected.n2)
test.factorize(list(n=3), factorize.expected.n3)
# test.factorize(list(n=999983), factorize.expected.prm)
# test.factorize(list(n=9999889*9999901), factorize.expected.pr2)
# test.factorize(list(n=4225390009), factorize.expected.prp)
# test.factorize(list(n=2^5 * 3^4 * 5^3 * 7^2 * 11), factorize.expected.nn)
# test.factorize(list(n=99989*99991*100003), factorize.expected.nm)
# test.factorize(list(n=2^32-1), factorize.expected.n32)
