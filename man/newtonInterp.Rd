\name{newtonInterp}
\alias{newtonInterp}
\alias{lagrangeInterp}
\title{
  Lagrange and Newtons Interpolation
}
\description{
  Lagrange's and Newton's method of polynomial interpolation.
}
\usage{
newtonInterp(x, y, xs = c())

lagrangeInterp(x, y, xs)
}
\arguments{
  \item{x, y}{x-, y-coordinates of data points defining the polynomial.}
  \item{xs}{either empty, or a vector of points to be interpolated.}
}
\details{
  Straightforward implementation of Lagrange's Newton's method
  (vectorized in \code{xs}).
}
\value{
  A vector of values at \code{xs} of the polynomial defined by \code{x,y}.
}
\references{
  Each textbook on numerical analysis.
}
\seealso{
  \code{\link{neville}}, \code{\link{barylag}}
}
\examples{
p <- Poly(c(1, 2, 3))
fp <- function(x) polyval(p, x)

x <- 0:4; y <- fp(x)
xx <- linspace(0, 4, 51)
yy <- lagrangeInterp(x, y, xx)
yy <- newtonInterp(x, y, xx)
\dontrun{
ezplot(fp, 0, 4)
points(xx, yy)}
}
\keyword{ math }
