\name{romberg}
\alias{romberg}
\title{
  Romberg Integration
}
\description{
  Romberg Integration
}
\usage{
romberg(f, a, b, tol = .Machine$double.eps^(2/3), ...)
}
\arguments{
  \item{f}{function to be integrated.}
  \item{a, b}{end points of the interval.}
  \item{tol}{requested tolerance.}
  \item{...}{variables to be passed to the function.}
}
\details{
  Simple Romberg integration with an explicit Richardson method applied
  to a series of trapezoidal integrals.
}
\value{
  List of value and relative error.
}
\references{
  Mathews, J. H., and K. D. Fink (1999). Numerical Methods Using Matlab.
  Third Edition, Prentice Hall.
}
\note{
  The function does \emph{not} need to be vectorized.
  The relative error can sometimes be scaringly high. It may help
  to interchange the integration limits.
}
\seealso{
  \code{\link{gauss_kronrod}}
}
\examples{
romberg(sin, 0, pi)  #  2.000000000000001 , rel.error: 6.6e-13
romberg(exp, 0, 1)   #  1.718281828459044 , rel.error: 3.6e-13
                     #  1.718281828459045 , i.e. exp(1) - 1
}
\keyword{ math }
