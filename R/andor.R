##
##  a n d o r . R  Logical AND, OR
##


And <- function(l, k) {
    return((l & k) + 0)
}


Or <- function(l, k) {
    return((l | k) + 0)
}


# and <- function(...) {
# 	all(sapply(as.list(...), as.logical))
# }
# 
# or <- function(...)  {
# 	any(sapply(as.list(...), as.logical))
# }

