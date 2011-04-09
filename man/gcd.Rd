\name{GCD, LCM}
\alias{GCD}
\alias{LCM}
\title{GCD and LCM Integer Functions}
\description{
  Greatest common divisor and least common multiple
}
\usage{
GCD(n, m)
LCM(n, m)
}
\arguments{
  \item{n, m}{integer scalars}
}
\details{
  Computation based on the extended Euclidean algorithm in \code{extGCD}.
}
\value{
  a numeric (integer) value
}
\note{
  The following relation is always true:

  \code{n * m = GCD(n, m) * LCM(n, m)}
}
\seealso{
\code{\link{extGCD}}, \code{\link{coprime}}
}
\examples{
GCD(12, 10)
GCD(46368, 75025)  # Fibonacci numbers are relatively prime to each other
LCM(12, 10)
LCM(46368, 75025)  # = 46368 * 75025
}
\keyword{ arith }
