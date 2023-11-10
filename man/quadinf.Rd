\name{quadinf}
\alias{quadinf}
\title{
Infinite Integrals
}
\description{
  Iterative quadrature of functions over finite, semifinite, or
  infinite intervals.
}
\usage{
quadinf(f, xa, xb, tol = 1e-12, ...)
}
\arguments{
  \item{f}{univariate function; needs not be vectorized.}
  \item{xa}{lower limit of integration; can be infinite}
  \item{xb}{upper limit of integration; can be infinite}
  \item{tol}{accuracy requested.}
  \item{\dots}{additional arguments to be passed to \code{f}.}
}
\details{
  \code{quadinf} implements the `double exponential method' for fast 
  numerical integration of smooth real functions on finite intervals.
  For infinite intervals, the tanh-sinh quadrature scheme is applied,
  that is the transformation \code{g(t)=tanh(pi/2*sinh(t))}.

  Please note that this algorithm does work very accurately for
  `normal' function, but should not be applied to (heavily)
  oscillating functions. The maximal number of iterations is 7, so
  if this is returned the iteration may not have converged.

  The integrand function needs \emph{not} be vectorized.
}
\value{
  A list with components \code{Q} the integral value, \code{relerr}
  the relative error, and \code{niter} the number of iterations.
}
\note{
  See also my remarks on R-help in September 2010 in the thread
  ``bivariate vector numerical integration with infinite range''.
}
\references{
  D. H. Bayley. Tanh-Sinh High-precision Quadrature. 2006.\cr
  URL: https://www.davidhbailey.com//dhbpapers/dhb-tanh-sinh.pdf
}
\seealso{
  \code{\link{integrate}}, \code{\link{quadgk}}
}
\examples{
##  We will look at the error function exp(-x^2)
f <- function(x) exp(-x^2)          # sqrt(pi)/2         theory
quadinf(f, 0, Inf)                  # 0.8862269254527413
quadinf(f, -Inf, 0)                 # 0.8862269254527413

f = function(x) sqrt(x) * exp(-x)   # 0.8862269254527579 exact
quadinf(f, 0, Inf)                  # 0.8862269254527579

f = function(x) x * exp(-x^2)       # 1/2
quadinf(f, 0, Inf)                  # 0.5

f = function(x) 1 / (1+x^2)         # 3.141592653589793 = pi
quadinf(f, -Inf, Inf)               # 3.141592653589784
}
\keyword{ math }
