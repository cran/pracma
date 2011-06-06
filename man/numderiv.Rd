\name{numderiv}
\alias{numderiv}
\alias{numdiff}
\title{
  Richardson's Numerical Derivative
}
\description{
  Richardson's method applied to the computation of the numerical derivative.
}
\usage{
numderiv(f, x0, maxiter = 16, h = 1, ..., accuracy = .Machine$double.eps)

numdiff(f, x, maxiter = 16, h = 1, ..., accuracy = .Machine$double.eps)
}
\arguments{
  \item{f}{function to be differentiated.}
  \item{x0, x}{point(s) at which the derivative is to be computed.}
  \item{maxiter}{maximum number of iterations.}
  \item{h}{starting step size, should be the default \code{h=1}.}
  \item{accuracy}{accuracy with which the function \code{f} can be computed.}
  \item{...}{variables to be passed to function \code{f}.}
}
\details{
  Returns the derivative of \code{f} at \code{x0} resp. at all points of
  the vector \code{x}.
}
\value{
  Numeric scalar or vector of approximated derivatives.
}
\references{
  Mathews, J. H., and K. D. Fink (1999). Numerical Methods Using Matlab.
  Third Edition, Prentice Hall.
}
\author{
  This version is adapted from a textbook Matlab version of Mathews and Fink.

  HwB  email: <hwborchers@googlemail.com>
}
\note{
  See \code{grad} in the `numDeriv' package for another implementation of
  Richardson's method in the context of numerical differentiation.
}
\seealso{
  \code{\link{fderiv}}, \code{\link{complexstep}}
}
\examples{
# Differentiate an anti-derivative function
f <- function(x) sin(x)*sqrt(1+sin(x))
F <- function(x)
        integrate(f, 0, x, rel.tol = 1e-12)$value
x0 <- 1
dF0 <- numderiv(F, x0, accuracy = 6.5e-15)  #=> 1.141882942715462
f(x0)                                       #   1.141882942715464 true value
# fderiv(F, x0)                             #   1.141882942704476
# numDeriv::grad(F, x0)                     #   1.141882942705797

# Compare over a whole period
x <- seq(0, 2*pi, length.out = 11)
max(abs(numdiff(sin, x) - cos(x)))          #=> 1.61e-15
# max(abs(numDeriv::grad(sin, x) - cos(x))) #   7.70e-12
}
\keyword{ math }
