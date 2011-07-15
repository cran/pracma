\name{froots}
\alias{froots}
\title{
  Find All Roots
}
\description{
  Finding all roots of a unvariate function in an interval by splitting
  the interval in many small subintervals.
}
\usage{
froots(f, a, b, n = 100, tol = .Machine$double.eps^(2/3), ...)
}
\arguments{
  \item{f}{functions whose roots shall be found.}
  \item{a, b}{endpoints of the interval.}
  \item{n}{number of subintervals to generate and search.}
  \item{tol}{tolerance for identifying zeros.}
  \item{\ldots}{Additional parameters to be passed to the function.}
}
\details{
  Roots, i.e. zeros in a subinterval will be found by applying \code{uniroot}
  to any subinterval where the sign of the function changes. The endpoints of
  the interval will be tested separately.

  The function need not be vectorized.
}
\value{
  Numeric vector with the x-positions of all roots found in the interval.
}
\seealso{
  \code{\link{uniroot}}
}
\examples{
f1 <- function(x) sin(pi/x)
froots(f1, 1/10, 1)
#  0.1000000  0.1111028  0.1250183  0.1428641  0.1666655
#  0.2000004  0.2499867  0.3333441  0.4999794  1.0000000

f2 <- function(x) 0.5*(1 + sin(10*pi*x))
froots(f2, 0, 1)
#  0.15  0.35  0.55  0.75  0.95
}
\keyword{ math }
