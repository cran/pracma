##
##  s i n e d . R  Trigonometric Functions with degrees
##


sind <- function(x)  sinpi( x / 180 )
cosd <- function(x)  cospi( x / 180 )
tand <- function(x)  sinpi(x/180) / cospi(x/180)
cotd <- function(x)  1 / tand(x)

asind <- function(x)  asin(x) * 180 / pi
acosd <- function(x)  acos(x) * 180 / pi
atand <- function(x)  atan(x) * 180 / pi
acotd <- function(x)  atand(1 / x)

atan2d <- function(x1, x2)  atan2(x1, x2) * 180 / pi

secd  <- function(x)  1 / cosd(x)
cscd  <- function(x)  1 / sind(x)
asecd <- function(x)  asec(x) * 180 / pi
acscd <- function(x)  acsc(x) * 180 / pi
