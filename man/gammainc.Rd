\name{gammainc}
\alias{gammainc}
\title{
  Incomplete Gamma Function
}
\description{
  Lower and upper incomplete gamma function.
}
\usage{
gammainc(x, a)
}
\arguments{
  \item{x}{real number, not too big.}
  \item{a}{real positive number.}
}
\details{
  Computes the lower and upper incomplete gamma function, including the
  regularized gamma function. The lower and upper incomplete gamma functions
  are defined as 
  \deqn{\gamma(x, a) = \int_0^x e^{-t} \, t^{a-1} \, dt}
  and
  \deqn{\Gamma(x, a) = \int_x^{\infty} e^{-t} \, t^{a-1} \, dt}
  while the regularized incomplete gamma function is
  \eqn{\gamma(x, a)/\Gamma(a)}.

  Accuracy is 7 significant digits along the real axis.
}
\value{
  Returns a list with the values of the lower, the upper, and regularized 
  lower incomplete gamma function.
}
\references{
  Zhang, Sh., and J. Jin (1996). Computation of Special Functions.
  Wiley-Interscience, New York.
}
\note{
  Directly converting Fortran code is often easier than translating Matlab
  code generated with f2matlab.
}
\seealso{
  \code{\link{gamma}}
}
\examples{
gammainc( 1.5, 2)
gammainc(-1.5, 2)
}
\keyword{ specfun }
