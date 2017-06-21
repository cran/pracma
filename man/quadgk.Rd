\name{quadgk}
\alias{quadgk}
\title{
  Adaptive Gauss-Kronrod Quadrature
}
\description{
  Adaptive Gauss-Kronrod Quadrature.
}
\usage{
quadgk(f, a, b, tol = .Machine$double.eps^0.5, ...)
}
\arguments{
  \item{f}{integrand as function; needs to be vectorized,
           but may have singularities at the endpoints.}
  \item{a, b}{endpoints of the integration interval.}
  \item{tol}{relative tolerence.}
  \item{\ldots}{Additional parameters to be passed to the function f.}
}
\details{
  Adaptive version of the (7, 15)-point Gauss-Kronrod quadrature formula,
  where in each recursion the error is taken as the difference between these
  two estimated integrals.

  The function \code{f} must be vectorized, though this will not be checked
  and may lead to strange errors. If it is not, use \code{F = Vectorize(f)}.
}
\value{
  Value of the integration. The relative error should be of the same
  order of magnitude as the relative tolerance (or much smaller).
}
\note{
  Uses the same nodes and weights as the \code{quadQK15} procedure in the
  QUADPACK library.
}
\seealso{
  \code{gauss_kronrod}
}
\examples{
##  Dilogarithm function
flog <- function(t) log(1-t)/t
quadgk(flog, 1, 0, tol = 1e-12)
# 1.644934066848128 - pi^2/6 < 1e-13
}
\keyword{ math }
