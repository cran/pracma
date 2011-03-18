\name{ifactor}
\alias{ifactor}
\title{Prime Factors}
\description{
  Returns a vector containing the prime ifactor of \code{n}.
}
\usage{
  ifactor(n)
}
\arguments{
  \item{n}{nonnegative integer}
}
\details{
  Computes the prime factors of \code{n} in ascending order,
  each one as often as its multiplicity requires, such that 
  \code{n == prod(ifactor(n))}.

  The corresponding Matlab function is called 'factor', but because
  factors have a special meaning in R and the factor() funtion in R
  could not (or should not) be shadowed, the number theoretic function
  has been renamed here.
}
\value{
  vector containing the prime factors of \code{n}.
}
\author{
  hwb \email{hwborchers@googlemail.com}
}
\seealso{
  \code{\link{isprime}, \link{primes}}
}
\examples{
\dontrun{
  ifactor(1002001)       # 7  7  11  11  13  13
  ifactor(65537)         # is prime
  # Euler's calculation
  ifactor(2^32 + 1)      # 641  6700417}
}
\keyword{ math }
