\name{quad2d}
\alias{quad2d}
\title{
  2-d Gaussian Quadrature
}
\description{
  Two-dimensional Gaussian Quadrature.
}
\usage{
quad2d(f, xa, xb, ya, yb, n = 32, ...)
}
\arguments{
  \item{f}{function of two variables; needs to be vectorized.}
  \item{xa, ya}{lower limits of integration; must be finite.}
  \item{xb, yb}{upper limits of integration; must be finite.}
  \item{n}{number of nodes used per direction.}
  \item{\dots}{additional arguments to be passed to \code{f}.}
}
\details{
  Extends the Gaussian quadrature to two dimensions by computing two sets
  of nodes and weights (in x- and y-direction), evaluating the function
  on this grid and multiplying weights appropriately.

  The function \code{f} needs to be vectorized in both variables such that
  \code{f(X, Y)} returns a matrix when \code{X} an \code{Y} are matrices
  (of the same size).

  \code{quad} is not suitable for functions with singularities.
}
\value{
  A single numerical value, the computed integral.
}
\references{
  Quarteroni, A., R. Sacco, and F. Saleri (2007). Numerical Mathematics.
  Second Edition, Springer-Verlag, Berlin Heidelberg.
}
\note{
  The extension of Gaussian quadrature to two dimensions is obvious, but
  see also the example `integral2d.m' at Nick Trefethens ``10 digits 1 page''.
}
\seealso{
\code{\link{quad}}, \code{cubature::adaptIntegrate}
}
\examples{
##  Example:  f(x, y) = (y+1)*exp(x)*sin(16*y-4*(x+1)^2)
f <- function(x, y)
        (y+1) * exp(x) * sin(16*y-4*(x+1)^2)
# this is even faster than cubature::adaptIntegral():
quad2d(f, -1, 1, -1, 1)
# 0.0179515583236958  # true value 0.01795155832370

##  Volume of the sphere: use polar coordinates
f0 <- function(x, y) sqrt(1 - x^2 - y^2)  # for x^2 + y^2 <= 1
fp <- function(x, y) y * f0(y*cos(x), y*sin(x))
quad2d(fp, 0, 2*pi, 0, 1, n = 101)  # 2.09439597740074
2/3 * pi                            # 2.0943951023932
}
\keyword{ math }
