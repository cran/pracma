\name{omega}
\alias{omega}
\alias{Omega}
\title{Number of Prime Factors}
\description{
  Sum of all exponents of prime factors in the prime decomposition.
}
\usage{
omega(n)
Omega(n)
}
\arguments{
  \item{n}{Positive integer.}
}
\details{
  Compute the number of prime factors of \code{n} resp. the sum of their
  exponents in the prime decomposition.

  \code{(-1)^Omega(n)} is the Liouville function.
}
\value{
  Natural number, some of exponents.
}
\author{
  HwB <hwborchers@googlemail.com>
}
\note{
  Works well up to \code{10^9}.
}
\seealso{
\code{\link{factorize}}, \code{\link{sigma}}
}
\examples{
omega(2*3*5*7*11*13*17*19)  #=> 8
Omega(2 * 3^2 * 5^3 * 7^4)  #=> 10
}
\keyword{ math }
