\name{gcd, lcm}
\alias{gcd}
\alias{lcm}
\title{GCD and LCM Integer Functions}
\description{
  Greatest common divisor and least common multiple
}
\usage{
gcd(n, m)
lcm(n, m)
}
\arguments{
  \item{n, m}{integer scalars}
}
\details{
  Computation based on the extended Euclidean algorithm in \code{extgcd}.
}
\value{
  a numeric (integer) value
}
\note{
  The following relation is always true:

  \code{n * m = gcd(n, m) * lcm(n, m)}
}
\seealso{
\code{\link{extgcd}}, \code{\link{coprime}}
}
\examples{
gcd(12, 10)
gcd(46368, 75025)  # Fibonacci numbers are relatively prime to each other
lcm(12, 10)
lcm(46368, 75025)  # = 46368 * 75025
}
\keyword{ arith }
