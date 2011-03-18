\name{primes}
\alias{primes}
\title{Prime Numbers}
\description{
  Generate a list of prime numbers less or equal \code{n}.
}
\usage{
  primes(n)
}
\arguments{
  \item{n}{nonnegative integer greater than 1.}
}
\details{
  The list of prime numbers up to \code{n} is generated using the "sieve of
  Erasthostenes". This approach is reasonably fast, but may require a lot of
  main memory when \code{n} is large.

  In double precision arithmetic integers are represented exactly only up to
  2^53 - 1, therefore this is the maximal allowed value.
}
\value{
  vector of integers representing prime numbers
}
\author{
  hwb \email{hwborchers@googlemail.com}
}
\seealso{
  \code{\link{isprime}, \link{ifactor}}
}
\examples{
  primes(1000)
  \dontrun{
  length(primes(1e6))  # there are 78498 prime numbers less than 1,000,000.}
}
\keyword{ math }
