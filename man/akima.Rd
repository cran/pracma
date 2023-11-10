\name{akimaInterp}
\alias{akimaInterp}
\title{
  Univariate Akima Interpolation
}
\description{
  Interpolate smooth curve through given points on a plane.
}
\usage{
  akimaInterp(x, y, xi)
}
\arguments{
  \item{x, y}{x/y-coordinates of (irregular) grid points defining the curve.}
  \item{xi}{x-coordinates of points where to interpolate.}
}
\details{
  Implementation of Akima's univariate interpolation method, built from
  piecewise third order polynomials. There is no need to solve large systems
  of equations, and the method is therefore computationally very efficient.
}
\value{
  Returns the interpolated values at the points \code{xi} as a vector.
}
\note{
  There is also a 2-dimensional version in package `akima'.
}
\author{
  Matlab code by H. Shamsundar under BSC License; re-implementation in R
  by Hans W Borchers.
}
\references{
  Akima, H. (1970). A New Method of Interpolation and Smooth Curve Fitting
  Based on Local Procedures. Journal of the ACM, Vol. 17(4), pp 589-602.

  Hyman, J. (1983). Accurate Monotonicity Preserving Cubic Interpolation.
  SIAM J. Sci. Stat. Comput., Vol. 4(4), pp. 645-654.

  Akima, H. (1996). Algorithm 760: Rectangular-Grid-Data Surface Fitting that
  Has the Accurancy of a Bicubic Polynomial. ACM TOMS Vol. 22(3), pp. 357-361.

  Akima, H. (1996). Algorithm 761: Scattered-Data Surface Fitting that
  Has the Accuracy of a Cubic Polynomial. ACM TOMS, Vol. 22(3), pp. 362-371.
}
\seealso{
  \code{\link{kriging}}, \code{akima::aspline}, \code{akima::interp}
}
\examples{
x <- c( 0,  2,  3,  5,  6,  8,  9,   11, 12, 14, 15)
y <- c(10, 10, 10, 10, 10, 10, 10.5, 15, 50, 60, 85)
xs <- seq(12, 14, 0.5)          # 12.0 12.5     13.0     13.5     14.0
ys <- akimaInterp(x, y, xs)     # 50.0 54.57405 54.84360 55.19135 60.0
xs; ys

\dontrun{
plot(x, y, col="blue", main = "Akima Interpolation")
xi <- linspace(0,15,51)
yi <- akimaInterp(x, y, xi)
lines(xi, yi, col = "darkred")
grid()}
}
\keyword{ fitting }
