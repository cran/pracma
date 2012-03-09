\name{erf}
\alias{erf}
\alias{erfinv}
\alias{erfc}
\alias{erfcinv}
\alias{erfcx}
\alias{erfz}
\title{
  Error Functions and Inverses (Matlab Style)
}
\description{
  The error or Phi function is a variant of the cumulative normal (or
  Gaussian) distribution.
}
\usage{
erf(x)
erfinv(y)
erfc(x)
erfcinv(y)
erfcx(x)

erfz(z)
}
\arguments{
  \item{x, y}{vector of real numbers.}
  \item{z}{real or complex number; must be a scalar.}
}
\details{
  \code{erf} and \code{erfinv} are the error and inverse error functions.\cr
  \code{erfc} and \code{erfcinv} are the complementary error function and
  its inverse.\cr
  \code{erfcx} is the scaled complementary error function.
}
\value{
  Real or complex number, the value of the function.
  Please note that \code{erfz} is not (yet) vectorized.
}
\note{
  For the complex error function we used Fortran code from the book 
  S. Zhang & J. Jin ``Computation of Special Functions'' (Wiley, 1996).
}
\seealso{
  \code{\link{pnorm}}
}
\examples{
  x <- 1
  erf(x); 2*pnorm(sqrt(2)*x) - 1
# [1] 0.842700792949715
# [1] 0.842700792949715
  erfc(x); 1 - erf(x); 2*pnorm(-sqrt(2)*x)
# [1] 0.157299207050285
# [1] 0.157299207050285
# [1] 0.157299207050285
  erfz(1)
# [1] 0.842700792949715
}
\keyword{ stat }
