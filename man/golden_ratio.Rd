\name{golden_ratio}
\alias{golden_ratio}
\title{
  Golden Ratio Search
}
\description{
  Golden Ratio search for a univariate function minimum in a bounded interval.
}
\usage{
golden_ratio(f, a, b, ..., maxiter = 100, tol = .Machine$double.eps^0.5)
}
\arguments{
  \item{f}{Function or its name as a string.}
  \item{a, b}{endpoints of the interval.}
  \item{maxiter}{maximum number of iterations.}
  \item{tol}{absolute tolerance; default \code{sqrt(eps)}.}
  \item{...}{Additional arguments to be passed to f.}
}
\details{
 `Golden ratio' search for a univariate function minimum in a bounded interval.
}
\value{
  Return a list with components \code{xmin}, \code{fmin}, 
  the function value at the minimum, \code{niter}, the number of iterations
  done, and the estimated precision \code{estim.prec}
}
\seealso{
\code{\link{uniroot}}
}
\examples{
f <- function(x) x * cos(0.1*exp(x)) * sin(0.1*pi*exp(x))
golden_ratio(f, 0, 4, tol=10^-10)  # $xmin    = 3.24848329206212
optimize(f, c(0,4), tol=10^-10)    # $minimum = 3.24848328971188
}
\keyword{ optimize }
