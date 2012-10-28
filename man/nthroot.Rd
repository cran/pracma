\name{nthroot}
\alias{nthroot}
\title{Real nth Root}
\description{
  Compute the real n-th root of real numbers.
}
\usage{
  nthroot(x, n)
}
\arguments{
  \item{x}{numeric vector or matrix}
  \item{n}{positive integer specifying the exponent \eqn{1/n}.}
}
\details{
  Computes the n-th root real numbers of a numeric vector \code{x},
  while \code{x^(1/n)} will return \code{NaN} for negative numbers,
  even in case \code{n} is odd. If some numbers in \code{x} are negative,
  \code{n} must be odd. (This is different in \emph{Octave})
}
\value{
  Returns a numeric vector of solutions to \eqn{x^{1/n}}.
}
\seealso{
  \code{\link{sqrt}}
}
\examples{
  nthroot(c(1, -2, 3), 3)  #=> 1.000000 -1.259921  1.442250
  (-2)^(1/3)               #=> NaN
}
\keyword{ arith }
