\name{fibsearch}
\alias{fibsearch}
\title{
  Fibonacci Search
}
\description{
  Fibonacci search for function minimum.
}
\usage{
fibsearch(f, a, b, ..., endp = FALSE, tol = .Machine$double.eps^(1/2))
}
\arguments{
  \item{f}{Function or its name as a string.}
  \item{a, b}{endpoints of the interval}
  \item{endp}{logical; shall the endpoints be considered as possible minima?}
  \item{tol}{absolute tolerance; default \code{eps^(1/2)}.}
  \item{...}{Additional arguments to be passed to f.}
}
\details{
  Fibonacci search for a univariate function minimum in a bounded interval.
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
fibsearch(f, 0, 4, tol=10^-10)   # $xmin    = 3.24848329403424
optimize(f, c(0,4), tol=10^-10)  # $minimum = 3.24848328971188
}
\keyword{ optimize }
