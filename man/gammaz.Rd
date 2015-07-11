\name{gammaz}
\alias{gammaz}
\title{
  Complex Gamma Function
}
\description{
  Gamma function valid in the entire complex plane.
}
\usage{
gammaz(z)
}
\arguments{
  \item{z}{Real or complex number or a numeric or complex vector.}
}
\details{
  Computes the Gamma function for complex arguments using the Lanczos series
  approximation.

  Accuracy is 15 significant digits along the real axis and 13 significant
  digits elsewhere.

  To compute the logarithmic Gamma function use \code{log(gammaz(z))}.
}
\value{
  Returns a complex vector of function values.
}
\references{
  Zhang, Sh., and J. Jin (1996). Computation of Special Functions.
  Wiley-Interscience, New York.
}
\note{
  Copyright (c) 2001 Paul Godfrey for a Matlab version available on
  Mathwork's Matlab Central under BSD license.

  Numerical Recipes used a 7 terms formula for a less effective approximation.
}
\seealso{
  \code{\link{gamma}}, \code{gsl::lngamma_complex}
}
\examples{
max(gamma(1:10) - gammaz(1:10))
gammaz(-1)
gammaz(c(-2-2i, -1-1i, 0, 1+1i, 2+2i))

# Euler's reflection formula
z <- 1+1i
gammaz(1-z) * gammaz(z)  # == pi/sin(pi*z)
}
\keyword{ specfun }
