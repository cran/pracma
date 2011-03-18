##
##  f i n d . R  tests
##

find <- pracma::find

v <- c(3, 2, 1, 1, 2, 3)
identical(find(v == 1), c(3L, 4L))
v <- c(1, 0, 4, -3, 0, 0, 0, 8, 6)
identical(find(v), as.integer(c(1, 3, 4, 8, 9)))
identical(find(v > 2), c(3L, 8L, 9L))
identical(find(c()), integer(0))
identical(find(c(TRUE, FALSE, TRUE, FALSE, TRUE)), c(1L, 3L, 5L))
