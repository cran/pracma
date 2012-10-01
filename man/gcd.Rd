\name{gcd, lcm}
\alias{gcd}
\alias{Lcm}
\title{GCD and LCM Integer Functions}
\description{
  Greatest common divisor and least common multiple
}
\usage{
gcd(a, b, extended = FALSE)
Lcm(a, b)
}
\arguments{
  \item{a, b}{vectors of integers.}
  \item{extended}{logical; if \code{TRUE}
                           the extended Euclidean algorithm will be applied.}
}
\details{
  Computation based on the extended Euclidean algorithm.

  If both \code{a} and \code{b} are vectors of the same length, the greatest 
  common divisor/lowest common multiple will be computed elementwise.
  If one is a vektor, the other a scalar, the scalar will be replicated to
  the same length.
}
\value{
  A numeric (integer) value or vector of integers. Or a list of three vectors
  named \code{c, d, g}, g containing the greatest common divisors, such that

  \code{g = c * a + d * b}.
}
\note{
  The following relation is always true:

  \code{n * m = gcd(n, m) * lcm(n, m)}
}
\seealso{
\code{numbers::extGCD}
}
\examples{
gcd(12, 1:24)
gcd(46368, 75025)  # Fibonacci numbers are relatively prime to each other
Lcm(12, 1:24)
Lcm(46368, 75025)  # = 46368 * 75025
}
\keyword{ arith }
