\name{quadv}
\alias{quadv}
\title{
  Vectorized Integration
}
\description{
  Vectorized adaptive Simpson integration.
}
\usage{
quadv(f, a, b, tol = .Machine$double.eps^(1/2), ...)
}
\arguments{
  \item{f}{univariate, vector-valued function; need not be vectorized.}
  \item{a, b}{endpoints of the integration interval.}
  \item{tol}{acuracy required for the recursion step.}
  \item{\dots}{further parameters to be passed to the function \code{f}.}
}
\details{
  Recursive version of the adaptive Simpson quadrature, recursion is based
  on the maximum of all components of the function calls.

  \code{quad} is not suitable for functions with singularities in the
  interval or at end points.
}
\value{
  Returns a list with components \code{Q} the integral value, \code{fcnt}
  the number of function calls, and \code{estim.prec} the estimated precision
  that normally will be much too high.
}
\seealso{
  \code{\link{quad}}
}
\examples{
##  Examples
f1 <- function(x) c(sin(x), cos(x))
quadv(f1, 0, pi)
# $Q
#  [1] 2.000000e+00 1.110223e-16
# $fcnt
#  [1] 65
# $estim.prec
#  [1] 4.321337e-07

f2 <- function(x) x^c(1:10)
quadv(f2, 0, 1, tol = 1e-12)
# $Q
#  [1] 0.50000000 0.33333333 0.25000000 0.20000000 0.16666667
#  [6] 0.14285714 0.12500000 0.11111111 0.10000000 0.09090909
# $fcnt
#  [1] 505
# $estim.prec
#  [1] 2.49e-10
}
\keyword{ math }
