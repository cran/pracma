\name{erf}
\alias{erf}
\alias{erfinv}
\alias{erfc}
\alias{erfcinv}
\alias{erfcx}
\alias{erfz}
\alias{erfi}
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
erfi(z)
}
\arguments{
  \item{x, y}{vector of real numbers.}
  \item{z}{real or complex number; must be a scalar.}
}
\details{
  \code{erf} and \code{erfinv} are the error and inverse error functions.\cr
  \code{erfc} and \code{erfcinv} are the complementary error function and
  its inverse.\cr
  \code{erfcx} is the scaled complementary error function.\cr
  \code{erfz} is the complex, \code{erfi} the imaginary error function.
}
\value{
  Real or complex number(s), the value(s) of the function.
}
\author{
  First version by Hans W Borchers;
  vectorized version of \code{erfz} by Michael Lachmann.
}
\note{
  For the complex error function we used Fortran code from the book 
  S. Zhang & J. Jin ``Computation of Special Functions'' (Wiley, 1996).
}
\seealso{
  \code{\link{pnorm}}
}
\examples{
  x <- 1.0
  erf(x); 2*pnorm(sqrt(2)*x) - 1
# [1] 0.842700792949715
# [1] 0.842700792949715
  erfc(x); 1 - erf(x); 2*pnorm(-sqrt(2)*x)
# [1] 0.157299207050285
# [1] 0.157299207050285
# [1] 0.157299207050285
  erfz(x)
# [1] 0.842700792949715
  erfi(x)
# [1] 1.650425758797543
}
\keyword{ stat }
