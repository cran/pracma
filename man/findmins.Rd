\name{findmins}
\alias{findmins}
\title{
  Find All Minima
}
\description{
  Finding all local(!) minima of a unvariate function in an interval by
  splitting the interval in many small subintervals.
}
\usage{
findmins(f, a, b, n = 100, tol = .Machine$double.eps^(2/3), ...)
}
\arguments{
  \item{f}{functions whose minima shall be found.}
  \item{a, b}{endpoints of the interval.}
  \item{n}{number of subintervals to generate and search.}
  \item{tol}{has no effect at this moment.}
  \item{\ldots}{Additional parameters to be passed to the function.}
}
\details{
  Local minima are found by looking for one minimum in each subinterval.
  It will be found by applying \code{optimize} to any two adjacent
  subinterval where the first slope is negative and the second one
  positive.

  If the function is minimal on a whole subinterval, this will cause
  problems. If some minima are apparently not found, increase the number
  of subintervals.
  
  Note that the endpoints of the interval will never be considered to be
  local minima. The function need not be vectorized.
}
\value{
  Numeric vector with the x-positions of all minima found in the interval.
}
\seealso{
  \code{\link{optimize}}
}
\examples{
fun <- function(x) x * cos(0.1*exp(x)) * sin(0.1*pi*exp(x))
\dontrun{ezplot(fun, 0, 5, n = 1001)}

# If n is smaller, the rightmost minimum will not be found.
findmins(fun, 0, 5, n= 1000)
#  2.537727 3.248481 3.761840 4.023021 4.295831
#  4.455115 4.641481 4.756263 4.897461 4.987802
}
\keyword{ optimize }
