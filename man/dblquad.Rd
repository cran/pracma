\name{dblquad}
\alias{dblquad}
\title{
  Double Integration
}
\description{
  Numerically evaluate double integral over rectangle.
}
\usage{
dblquad(f, xa, xb, ya, yb, dim = 2, ..., 
        subdivs = 300, tol = .Machine$double.eps^0.5)
}
\arguments{
  \item{f}{function of two variables, the integrand.}
  \item{xa, xb}{left and right endpoint for first variable.}
  \item{ya, yb}{left and right endpoint for second variable.}
  \item{dim}{which variable to integrate first.}
  \item{subdivs}{number of subdivisions to use.}
  \item{tol}{relative tolerance to use in \code{integrate}.}
  \item{\ldots}{additional parameters to be passed to the integrand.}
}
\details{
  This function applies the internal single variable integration function
  \code{integrate} two times, once for each variable.
}
\value{
  Numerical scalar, the value of the integral.
}
\seealso{
  \code{\link{integrate}}, \code{\link{quad2d}}
}
\examples{
f1 <- function(x, y) x^2 + y^2
dblquad(f1, -1, 1, -1, 1)     #   2.666666667 , i.e. 8/3 . err = 0

f2 <- function(x, y) y*sin(x)+x*cos(y)
dblquad(f2, pi, 2*pi, 0, pi)  #  -9.869604401 , i.e. -pi^2, err = 0

# f3 <- function(x, y) sqrt((1 - (x^2 + y^2)) * (x^2 + y^2 <= 1))
f3 <- function(x, y) sqrt(pmax(0, 1 - (x^2 + y^2)))
dblquad(f3, -1, 1, -1, 1)     #   2.094395124 , i.e. 2/3*pi , err = 2e-8
}
\keyword{ math }
