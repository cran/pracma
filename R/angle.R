##
##  a n g l e . R
##


real <- function(z) Re(z)

imag <- function(z) Im(z)

conj <- function(z) Conj(z)

# use abs() for Mod()

angle <- function(z) atan2(Im(z), Re(z))
