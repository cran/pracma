\name{erf, erfc}
\alias{erf}
\alias{erfc}
\alias{erfz}
\title{
  Error Functions (Matlab Style)
}
\description{
  The error or Phi function is a variant of the cumulative normal (or
  Gaussian) distribution.
}
\usage{
erf(x)
erfc(x)

erfz(z)
}
\arguments{
  \item{x}{vector of real numbers.}
  \item{z}{real or complex number; must be a scalar.}
}
\details{
  For real arguments, the following obvious relations
  \deqn{erf(x) = 2 Phi(x \sqrt(2)) - 1}
  \deqn{erfc(x) = 2 Phi(-x \sqrt(2))}
  for the error and the complementary error functions were used.
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
