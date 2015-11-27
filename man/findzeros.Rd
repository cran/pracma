\name{findzeros}
\alias{findzeros}
\title{
  Find All Roots
}
\description{
  Finding all roots of a unvariate function in an interval by splitting
  the interval in many small subintervals.
}
\usage{
findzeros(f, a, b, n = 100, tol = .Machine$double.eps^(2/3), ...)
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

  If the function points are both positive or negative and the slope in this
  interval is high enough, the minimum or maximum will be determined with
  \code{optimize} and checked for a possible zero. 

  The function need not be vectorized.
}
\value{
  Numeric vector with the x-positions of all roots found in the interval.
}
\seealso{
  \code{\link{findmins}}
}
\examples{
f1 <- function(x) sin(pi/x)
findzeros(f1, 1/10, 1)
#  0.1000000  0.1111028  0.1250183  0.1428641  0.1666655
#  0.2000004  0.2499867  0.3333441  0.4999794  1.0000000

f2 <- function(x) 0.5*(1 + sin(10*pi*x))
findzeros(f2, 0, 1)
#  0.15  0.35  0.55  0.75  0.95

f3 <- function(x) sin(pi/x) + 1
findzeros(f3, 0.1, 0.5)
# 0.1052632 0.1333333 0.1818182 0.2857143

f4 <- function(x) sin(pi/x) - 1
findzeros(f4, 0.1, 0.5)
# 0.1176471 0.1538462 0.2222222 0.4000000

\dontrun{
# Dini function
Dini <- function(x) x * besselJ(x, 1) + 3 * besselJ(x, 0)
findzeros(Dini, 0, 100, n = 128)
ezplot(Dini, 0, 100, n = 512)
}
}
\keyword{ math }
