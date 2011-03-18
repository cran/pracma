\name{coprime}
\alias{coprime}
\title{Coprimality}
\description{
  Determine whether two numbers are coprime, i.e. do not have a common
  prime divisor.
}
\usage{
coprime(n,m)
}
\arguments{
  \item{n, m}{integer scalars}
}
\details{
  Two numbers are coprime iff their greatest common divisor is 1.
}
\value{
  Logical, being TRUE if the numbers are coprime.
}
\seealso{
\code{\link{gcd}}
}
\examples{
coprime(46368, 75025)  # Fibonacci numbers are relatively prime to each other
coprime(1001, 1334)
}
\keyword{ arith }
