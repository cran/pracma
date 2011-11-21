\name{primes}
\alias{primes}
\alias{primes2}
\alias{nextPrime}
\alias{twinPrimes}
\title{Prime Numbers}
\description{
  Generate a list of prime numbers less or equal \code{n}, resp. between
  \code{n1} and \code{n2}.
}
\usage{
  primes(n)
  primes2(n1, n2)

  nextPrime(n)
  twinPrimes(n1, n2)
}
\arguments{
  \item{n}{nonnegative integer greater than 1.}
  \item{n1, n2}{natural numbers with \code{n1 <= n2}.}
}
\details{
  The list of prime numbers up to \code{n} is generated using the "sieve of
  Erasthostenes". This approach is reasonably fast, but may require a lot of
  main memory when \code{n} is large.

  \code{primes2} computes first all primes up to \code{sqrt(n2)} and then
  applies a refined sieve on the numbers from \code{n1} to \code{n2}, thereby
  drastically reducing the need for storing long arrays of numbers.

  \code{nextPrime}  finds the next prime number greater than \code{n}. In
  general the next prime will occur in the interval \code{[n+1,n+log(n)]}.

  \code{twinPrimes} uses \code{primes2} and uses \code{diff} to find all
  twin primes in the given interval.

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
  \code{\link{isprime}, \link{factorize}}
}
\examples{
  primes(1000)
  primes2(1949, 2011)

  nextPrime(1e+6)
  twinPrimes(1e6+1, 1e6+1001)
  \dontrun{
  length(primes(1e6))  # there are 78498 prime numbers less than 1,000,000.}
}
\keyword{ math }
