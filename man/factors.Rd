\name{factors}
\alias{factors}
\title{Prime Factors}
\description{
  Returns a vector containing the prime factors of \code{n}.
}
\usage{
  factors(n)
}
\arguments{
  \item{n}{nonnegative integer}
}
\details{
  Computes the prime factors of \code{n} in ascending order,
  each one as often as its multiplicity requires, such that 
  \code{n == prod(factors(n))}.

  The corresponding Matlab function is called `factor', but because
  factors have a special meaning in R and the factor() function in R
  could not (or should not) be shadowed, the number theoretic function
  has been renamed here.
}
\value{
  Vector containing the prime factors of \code{n}.
}
\seealso{
  \code{\link{isprime}, \link{primes}}
}
\examples{
\dontrun{
  factors(1002001)       # 7  7  11  11  13  13
  factors(65537)         # is prime
  # Euler's calculation
  factors(2^32 + 1)      # 641  6700417}
}
\keyword{ math }
