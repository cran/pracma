\name{isprime}
\alias{isprime}
\title{isprime Property}
\description{
  Vectorized version, returning for a vector or matrix of positive integers
  a vector of the same size containing 1 for the elements that are prime and
  0 otherwise.
}
\usage{
  isprime(x)
}
\arguments{
  \item{x}{vector or matrix of nonnegative integers}
}
\details{
  Given an array of positive integers returns an array of the same size
  of 0 and 1, where the i indicates a prime number in the same position.
}
\value{
  array of elements 0, 1 with 1 indicating prime numbers
}
\seealso{
  \code{\link{factors}, \link{primes}}
}
\examples{
  x <- matrix(1:10, nrow=10, ncol=10, byrow=TRUE)
  x * isprime(x)

  # Find first prime number octett:
  octett <- c(0, 2, 6, 8, 30, 32, 36, 38) - 19
  while (TRUE) {
      octett <- octett + 210
      if (all(as.logical(isprime(octett)))) {
          cat(octett, "\n", sep="  ")
          break
      }
  }
}
\keyword{ math }
