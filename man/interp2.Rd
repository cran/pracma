\name{interp2}
\alias{interp2}
\title{
  Two-dimensional Data Interpolation
}
\description{
  Two-dimensional data interpolation similar to a table look-up.
}
\usage{
    interp2(x, y, Z, xp, yp, method = c("linear", "nearest", "constant"))
}
\arguments{
  \item{x, y}{vectors with monotonically increasing elements, representing
              x- and y-coordinates of the data values in \code{Z}.}
  \item{Z}{numeric \code{length(y)}-by-\code{length(x)} matrix.}
  \item{xp, yp}{x-, y-coordinates of points at which interpolated values
                will be computed.}
  \item{method}{interpolation method, ``linear'' the most useful.}
}
\details{
   Computes a vector containing elements corresponding to the elements of
   \code{xp} and \code{yp}, determining by interpolation within the
   two-dimensional function specified by vectors \code{x} and \code{y},
   and matrix \code{Z}.
   
   \code{x} and \code{y} must be monotonically increasing. They specify
   the points at which the data \code{Z} is given.
   Therefore, \code{length(x) = nrow(Z)} and \code{length(y) = ncol(Z)}
   must be satisfied.

  \code{xp} and \code{yp} must be of the same length.

  The functions appears vectorized as \code{xp}, \code{yp} can be
  vectors, but internally they are treated in a \code{for} loop.
}
\value{
  Vector the length of \code{xp} of interpolated values.

  For methods ``constant'' and ``nearest'' the intervals are considered
  closed from left and below. Out of range values are returned as NAs.
}
\note{
  The corresponding Matlab function has also the methods ``cubic'' and
  ``spline''. If in need of a nonlinear interpolation, take a look at
  \code{barylag2d} in this package and the example therein.
}
\seealso{
  \code{\link{interp1}}, \code{barylag2d}
}
\examples{
\dontrun{
    x <- linspace(-1, 1, 11)
    y <- linspace(-1, 1, 11)
    mgrid <- meshgrid(x, y)
    Z <- mgrid$X^2 + mgrid$Y^2
    xp <- yp <- linspace(-1, 1, 101)

    method <- "linear"
    zp <- interp2(x, y, Z, xp, yp, method)
    plot(xp, zp, type = "l", col = "blue")

    method = "nearest"
    zp <- interp2(x, y, Z, xp, yp, method)
    lines(xp, zp, col = "red")
    grid()}
}
\keyword{ math }
