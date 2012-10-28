\name{simpson2d}
\alias{simpson2d}
\title{
  Double Simpson Integration
}
\description{
  Numerically evaluate double integral by 2-dimensional Simpson method.
}
\usage{
simpson2d(f, xa, xb, ya, yb, nx = 128, ny = 128, ...)
}
\arguments{
  \item{f}{function of two variables, the integrand.}
  \item{xa, xb}{left and right endpoint for first variable.}
  \item{ya, yb}{left and right endpoint for second variable.}
  \item{nx, ny}{number of intervals in x- and y-direction.}
  \item{\ldots}{additional parameters to be passed to the integrand.}
}
\details{
  The 2D Simpson integrator has weights that are most easily determined 
  by taking the outer product of the vector of weights for the 1D Simpson
  rule.
}
\value{
  Numerical scalar, the value of the integral.
}
\note{
  Copyright (c) 2008 W. Padden and Ch. Macaskill for Matlab code published
  under BSD License on MatlabCentral.
}
\seealso{
  \code{\link{dblquad}}, \code{\link{quad2d}}
}
\examples{
f1 <- function(x, y) x^2 + y^2
simpson2d(f1, -1, 1, -1, 1)     #   2.666666667 , i.e. 8/3 . err = 0

f2 <- function(x, y) y*sin(x)+x*cos(y)
simpson2d(f2, pi, 2*pi, 0, pi)  #  -9.869604401 , i.e. -pi^2, err = 2e-8

f3 <- function(x, y) sqrt((1 - (x^2 + y^2)) * (x^2 + y^2 <= 1))
simpson2d(f3, -1, 1, -1, 1)     #   2.094393912 , i.e. 2/3*pi , err = 1e-6
}
\keyword{ math }
