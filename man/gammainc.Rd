\name{gammainc}
\alias{gammainc}
\alias{incgam}
\title{
  Incomplete Gamma Function
}
\description{
  Lower and upper incomplete gamma function.
}
\usage{
gammainc(x, a)

incgam(x, a)
}
\arguments{
  \item{x}{positive real number.}
  \item{a}{real number.}
}
\details{
  \code{gammainc} computes the lower and upper incomplete gamma
  function, including the regularized gamma function. The lower and
  upper incomplete gamma functions are defined as 
  \deqn{\gamma(x, a) = \int_0^x e^{-t} \, t^{a-1} \, dt}
  and
  \deqn{\Gamma(x, a) = \int_x^{\infty} e^{-t} \, t^{a-1} \, dt}
  while the regularized incomplete gamma function is
  \eqn{\gamma(x, a)/\Gamma(a)}.

  \code{incgam} (a name used in Pari/GP) computes the upper incomplete
  gamma function alone, applying the R function \code{pgamma}. The
  accuracy is thus much higher. It works for \code{a >= -1}, for even
  smaller values a recursion will give the result.
}
\value{
  \code{gammainc} returns a list with the values of the lower, the
  upper, and regularized lower incomplete gamma function.
  \code{incgam} only returns the value of the incomplete upper gamma
  function.
}
\references{
  Zhang, Sh., and J. Jin (1996). Computation of Special Functions.
  Wiley-Interscience, New York.
}
\note{
  Directly converting Fortran code is often easier than translating
  Matlab code generated with f2matlab.
}
\seealso{
  \code{\link{gamma}}, \code{\link{pgamma}}
}
\examples{
gammainc( 1.5, 2)
gammainc(-1.5, 2)

incgam(3, 1.2)
incgam(3, 0.5); incgam(3, -0.5)
}
\keyword{ specfun }
