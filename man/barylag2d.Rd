\name{barylag2d}
\alias{barylag2d}
\title{2-D Barycentric Lagrange Interpolation}
\description{
  Two-dimensional barycentric Lagrange interpolation.
}
\usage{
barylag2d(F, xn, yn, xf, yf)
}
\arguments{
  \item{F}{matrix representing values of a function in two dimensions.}
  \item{xn, yn}{x- and y-coordinates of supporting nodes.}
  \item{xf, yf}{x- and y-coordinates of an interpolating grid..}
}
\details{
  Well-known Lagrange interpolation using barycentric coordinates, here
  extended to two dimensions. The function is completely vectorized.

  x-coordinates run downwards in F, y-coordinates to the right. That conforms
  to the usage in image or contour plots, see the example below.
}
\value{
  Matrix of size \code{length(xf)}-by-\code{length(yf)} giving the interpolated
  values at al the grid points \code{(xf, yf)}.
}
\references{
  Berrut, J.-P., and L. Nick Trefethen (2004). ``Barycentric Lagrange
  Interpolation''. SIAM Review, Vol. 46(3), pp.501--517.
}
\note{
  Copyright (c) 2004 Greg von Winckel of a Matlab function under BSD license;
  translation to R by Hans W Borchers with permission.
}
\seealso{
  \code{\link{interp2}}, \code{\link{barylag}}
}
\examples{
##  Example from R-help
xn <- c(4.05, 4.10, 4.15, 4.20, 4.25, 4.30, 4.35)
yn <- c(60.0, 67.5, 75.0, 82.5, 90.0)
foo <- matrix(c(
        -137.8379, -158.8240, -165.4389, -166.4026, -166.2593,
        -152.1720, -167.3145, -171.1368, -170.9200, -170.4605,
        -162.2264, -172.5862, -174.1460, -172.9923, -172.2861,
        -168.7746, -175.2218, -174.9667, -173.0803, -172.1853,
        -172.4453, -175.7163, -174.0223, -171.5739, -170.5384,
        -173.7736, -174.4891, -171.6713, -168.8025, -167.6662,
        -173.2124, -171.8940, -168.2149, -165.0431, -163.8390),
            nrow = 7, ncol = 5, byrow = TRUE)
xf <- c(4.075, 4.1)
yf <- c(63.75, 67.25)
barylag2d(foo, xn, yn, xf, yf)
#  -156.7964 -163.1753
#  -161.7495 -167.0424

# Find the minimum of the underlying function
bar <- function(xy) barylag2d(foo, xn, yn, xy[1], xy[2])
optim(c(4.25, 67.5), bar)  # "Nelder-Mead"
# $par
# 4.230547 68.522747
# $value
# -175.7959

\dontrun{
# Image and contour plots
image(xn, yn, foo)
contour(xn, yn, foo, col="white", add = TRUE)
xs <- seq(4.05, 4.35, length.out = 51)
ys <- seq(60.0, 90.0, length.out = 51)
zz <- barylag2d(foo, xn, yn, xs, ys)
contour(xs, ys, zz, nlevels = 20, add = TRUE)
contour(xs, ys, zz, levels=c(-175, -175.5), add = TRUE)
points(4.23, 68.52)}
}
\keyword{ math }
