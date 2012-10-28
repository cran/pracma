\name{polylog}
\alias{polylog}
\title{
  Polylogarithm Function
}
\description{
  Computes the \code{n}-based polylogarithm of \code{z}: \code{Li_n(z)}.
}
\usage{
polylog(z, n)
}
\arguments{
  \item{z}{real number or vector, all entries satisfying \code{abs(z)<1}.}
  \item{n}{base of polylogarithm, integer greater or equal -4.}
}
\details{
  The Polylogarithm is also known as Jonquiere's function. It is defined as
  \deqn{\sum_{k=1}^{\infty}{z^k / k^n} = z + z^2/2^n + ...}

  The polylogarithm function arises, e.g., in Feynman diagram integrals. It
  also arises in the closed form of the integral of the Fermi-Dirac and the
  Bose-Einstein distributions.

  The special cases \code{n=2} and \code{n=3} are called the dilogarithm and 
  trilogarithm, respectively.

  Approximation should be correct up to at least 5 digits for \eqn{|z| > 0.55}
  and on the order of 10 digits for \eqn{|z| <= 0.55}.
}
\value{
  Returns the function value (not vectorized).
}
\note{
  Based on some equations, see references.
  A Matlab implementation is available in the Matlab File Exchange.
}
\references{
  V. Bhagat, et al. (2003). On the evaluation of generalized BoseEinstein and 
  FermiDirac integrals. Computer Physics Communications, Vol. 155, p.7.
}
\examples{
polylog(0.5,  1)    # polylog(z, 1) = -log(1-z)
polylog(0.5,  2)    # (p1^2 - 6*log(2)^2) / 12
polylog(0.5,  3)    # (4*log(2)^3 - 2*pi^2*log(2) + 21*zeta(3)) / 24
polylog(0.5,  0)    # polylog(z,  0) = z/(1-z)
polylog(0.5, -1)    # polylog(z, -1) = z/(1-z)^2
}
\keyword{ math }
