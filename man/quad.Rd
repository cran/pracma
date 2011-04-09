\name{quad}
\alias{quad}
\title{
Adaptive Simpson Quadrature
}
\description{
  Adaptive quadrature of functions of one variable over a finite interval.
}
\usage{
quad(f, xa, xb, tol = .Machine$double.eps^0.5, trace = FALSE, ...)
}
\arguments{
  \item{f}{a one-dimensional function; need not be vectorized.}
  \item{xa}{lower limit of integration; must be finite}
  \item{xb}{upper limit of integration; must be finite}
  \item{tol}{accuracy requested; default is \code{sqrt(.Machine.double.eps)}.}
  \item{trace}{logical; shall a trace be printed?}
  \item{\dots}{additional arguments to be passed to \code{f}.}
}
\details{
  Realizes adaptive Simpson quadrature in R through recursive calls.
}
\value{
  A single numeric value, the computed integral.
}
\references{
  Gander, W. and W. Gautschi (2000). ``Adaptive Quadrature --- Revisited''.
  BIT, Vol. 40, 2000, pp. 84-101. \url{http://www.inf.ethz.ch/personal/gander}
}
\author{
HwB  <hwborchers@googlemail.com>
}
\note{
  Copyright (c) 2000 Gander and Gautschi for the Matlab version.
  Translation to R by Hans W Borchers 2010.
}
\seealso{
\code{\link{integrate}}
}
\examples{
\dontrun{
# options(digits=15)
f <- function(x) x * cos(0.1*exp(x)) * sin(0.1*pi*exp(x))
quad(f, 0, 4)              # 1.2821290747821
quad(f, 0, 4, tol=10^-15)  # 1.2821290743501
integrate(f, 0, 4)
# 1.28212907435010 with absolute error < 4.1e-06

xx <- seq(0, 4, length.out = 200)
yy <- f(xx)
plot(xx, yy, type = 'l'); grid()
}
}
\keyword{ math }
