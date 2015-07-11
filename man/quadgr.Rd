\name{quadgr}
\alias{quadgr}
\title{
  Gaussian Quadrature with Richardson Extrapolation
}
\description{
  Gaussian 12-point quadrature with Richardson extrapolation.
}
\usage{
quadgr(f, a, b, tol = .Machine$double.eps^(1/2), ...)
}
\arguments{
  \item{f}{integrand as function, may have singularities at the endpoints.}
  \item{a, b}{endpoints of the integration interval.}
  \item{tol}{relative tolerence.}
  \item{\ldots}{Additional parameters to be passed to the function \code{f}.}
}
\details{
  \code{quadgr} uses a 12-point Gauss-Legendre quadrature.
  The error estimate is based on successive interval bisection. Richardson
  extrapolation accelerates the convergence for some integrals, especially
  integrals with endpoint singularities.

  Through some preprocessing infinite intervals can also be handled.
}
\value{
  List with \code{value} and \code{rel.err}.
}
\author{
  Copyright (c) 2009 Jonas Lundgren for the Matlab function \code{quadgr}
  available on MatlabCentral under the BSD license.
  
  R re-implementation by HwB, email: <hwborchers@googlemail.com>, in 2011.
}
\seealso{
  \code{gaussLegendre}
}
\examples{
##  Dilogarithm function
flog <- function(t) log(1-t)/t
quadgr(flog, 1, 0, tol = 1e-12)
# value
# 1.6449340668482 , is pi^2/6 = 1.64493406684823
# rel.err
# 2.07167616395054e-13
}
\keyword{ math }
