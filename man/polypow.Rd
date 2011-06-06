\name{polypow}
\alias{polypow}
\title{Polynomial Powers}
\description{
  Power of a polynomial.
}
\usage{
polypow(p, n)
}
\arguments{
  \item{p}{vector representing a polynomial.}
  \item{n}{positive integer, the exponent.}
}
\details{
  Uses \code{polymul} to multiply the polynomial \code{p} \code{n} times
  with itself.
}
\value{
  Vector representing a polynomial.
}
\note{
  There is no such function in Matlab or Octave.
}
\seealso{
  \code{\link{polymul}}
}
\examples{
polypow(c(1, -1), 6)             #=> (x - 1)^6 = (1  -6  15 -20  15  -6   1)
polypow(c(1, 1, 1, 1, 1, 1), 2)  # 1 2 3 4 5 6 5 4 3 2 1
}
\keyword{ math }
