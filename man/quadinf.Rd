\name{quadinf}
\alias{quadinf}
\title{
Infinite Integrals
}
\description{
  Adaptive quadrature of functions over an infinite interval.
}
\usage{
quadinf(f, xa, xb, tol = .Machine$double.eps^0.5,
            method = NULL, ...)
}
\arguments{
  \item{f}{univariate function; needs to be vectorized.}
  \item{xa}{lower limit of integration; can be infinite}
  \item{xb}{upper limit of integration; can be infinite}
  \item{tol}{accuracy requested.}
  \item{method}{select the integration method.}
  \item{\dots}{additional arguments to be passed to \code{f}.}
}
\details{
  \code{quadinf} is simply a wrapper for \code{integrate}. When one of
  the integration limits become infinite, the transformed function
  \code{(1/x^2)*f(1/x)} is used. This works fine if the new function
  does not have a too bad behavior at the limit(s).

  The function needs to be vectorized as long as this is required by
  \code{integrate}.

  Choices for adaptive integration methods are ``Kronrod", ``Richardson",
  ``Clenshaw", ``Simpson", or ``Romberg". If the method is \code{NULL} the
  standard integrate() function from R base will be used.
}
\value{
  A single numeric value, the computed integral.
}
\note{
  Based on my remarks on R-help in September 2010 in the thread
  ``bivariate vector numerical integration with infinite range''
}
\seealso{
\code{\link{integrate}}
}
\examples{
##  We will look at the error function exp(-x^2)
f <- function(x) exp(-x^2)
quadinf(f, -Inf, 0)  #=> 0.886226925756445 with abs. error 3e-10 (sqrt(pi)/2)
quadinf(f, 0, Inf)   # same
quadinf(f, -Inf, -1, tol = 1e-12) - integrate(f, -Inf, -1)$value
quadinf(f, -Inf,  1, tol = 1e-12) - integrate(f, -Inf,  1)$value
quadinf(f, -1,  Inf, tol = 1e-12) - integrate(f, -1,  Inf)$value
quadinf(f,  1,  Inf, tol = 1e-12) - integrate(f, -Inf, -1)$value
}
\keyword{ math }
