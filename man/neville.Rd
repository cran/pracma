\name{neville}
\alias{neville}
\title{
  Neville's Method
}
\description{
  Neville's's method of polynomial interpolation.
}
\usage{
neville(x, y, xs)
}
\arguments{
  \item{x, y}{x-, y-coordinates of data points defining the polynomial.}
  \item{xs}{single point to be interpolated.}
}
\details{
  Straightforward implementation of Neville's method; not yet vectorized.
}
\value{
  Interpolated value at \code{xs} of the polynomial defined by \code{x,y}.
}
\references{
  Each textbook on numerical analysis.
}
\seealso{
  \code{\link{newtonInterp}}, \code{\link{barylag}}
}
\examples{
p <- Poly(c(1, 2, 3))
fp <- function(x) polyval(p, x)

x <- 0:4; y <- fp(x)
xx <- linspace(0, 4, 51)
yy <- numeric(51)
for (i in 1:51) yy[i] <- neville(x, y, xx[i])

\dontrun{
ezplot(fp, 0, 4)
points(xx, yy)}
}
\keyword{ math }
