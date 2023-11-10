\name{quadl}
\alias{quadl}
\title{
Adaptive Lobatto Quadrature
}
\description{
  Adaptive quadrature of functions of one variable over a finite interval.
}
\usage{
quadl(f, xa, xb, tol = .Machine$double.eps^0.5, trace = FALSE, ...)
}
\arguments{
  \item{f}{a one-dimensional function; needs to be vectorized.}
  \item{xa}{lower limit of integration; must be finite}
  \item{xb}{upper limit of integration; must be finite}
  \item{tol}{accuracy requested.}
  \item{trace}{logical; shall a trace be printed?}
  \item{\dots}{additional arguments to be passed to \code{f}.}
}
\details{
  Realizes adaptive Lobatto quadrature in R through recursive calls.

  The function \code{f} needs to be vectorized though this could be changed
  easily.
}
\value{
  A single numeric value, the computed integral.
}
\references{
  Gander, W. and W. Gautschi (2000). ``Adaptive Quadrature --- Revisited''.
  BIT, Vol. 40, 2000, pp. 84-101.
}
\author{
  Copyright (c) 1998 Walter Gautschi for the Matlab version published as
  part of the referenced article. R implementation by Hans W Borchers 2011.
}
\note{
Compared to Gaussian quadrature, Lobatto integration include the end points
of the integration interval. It is accurate for polynomials up to degree
2n-3, where n is the number of integration points.
}
\seealso{
\code{\link{quad}}
}
\examples{
# options(digits=15)
f <- function(x) x * cos(0.1*exp(x)) * sin(0.1*pi*exp(x))
quadl(f, 0, 4)              # 1.2821290743501
integrate(f, 0, 4)
# 1.28212907435010 with absolute error < 4.1e-06

\dontrun{
xx <- seq(0, 4, length.out = 200)
yy <- f(xx)
plot(xx, yy, type = 'l')
grid()}
}
\keyword{ math }
