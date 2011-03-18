\name{extgcd}
\alias{extgcd}
\title{Extended Euclidean Algorithm}
\description{
  The extended Euclidean algorithm computes the greatest common divisor and
  solves Bezout's identity.
}
\usage{
extgcd(a, b)
}
\arguments{
  \item{a, b}{integer scalars}
}
\details{
  The extended Euclidean algorithm not only computes the greatest common
  divisor \eqn{d} of \eqn{a} and \eqn{b}, but also two numbers \eqn{n} and 
  \eqn{m} such that \eqn{d = n a + m b}.

  This algorithm provides an easy approach to computing the modular inverse.
}
\value{
  a numeric vector of length three, \code{c(d, n, m)}, where \code{d} is the
  greatest common divisor of \code{a} and \code{b}, and \code{n} and \code{m}
  are integers such that \code{d = n*a + m*b}.
}
\note{
  There is also a shorter, more elegant recursive version for the extended
  Euclidean algorithm. For R the procedure suggested by Blankinship appeared
  more appropriate.
}
\seealso{
\code{\link{modinv}}
}
\references{
  Blankinship, W. A. "A New Version of the Euclidean Algorithm."
  Amer. Math. Monthly 70, 742-745, 1963.
}
\examples{
gcd(12, 10)
gcd(46368, 75025)  # Fibonacci numbers are relatively prime to each other
lcm(12, 10)
lcm(46368, 75025)  # = 46368 * 75025
}
\keyword{ arith }
