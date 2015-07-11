\name{romberg}
\alias{romberg}
\title{
  Romberg Integration
}
\description{
  Romberg Integration
}
\usage{
romberg(f, a, b, maxit = 25, tol = 1e-12, ...)
}
\arguments{
  \item{f}{function to be integrated.}
  \item{a, b}{end points of the interval.}
  \item{maxit}{maximum number of iterations.}
  \item{tol}{requested tolerance.}
  \item{...}{variables to be passed to the function.}
}
\details{
  Simple Romberg integration with an explicit Richardson method applied
  to a series of trapezoidal integrals. This scheme works best with smooth
  and non-oscillatory functions and needs the least number of function calls
  among all integration routines.

  The function does \emph{not} need to be vectorized.
}
\value{
  List of value, number or iterations, and relative error.
}
\references{
  Chapra, S. C., and R. P. Canale (2006). Numerical Methods for Engineers.
  Fifth Edition, McGraw-Hill, New York.
}
\note{
  Using a trapezoid formula Romberg integration will use
  \code{2*(2^iter-1)+iter} function calls. By remembering function values
  this could be reduced to \code{2^iter+1} calls.
}
\seealso{
  \code{\link{integrate}}, \code{\link{quadgr}}
}
\examples{
romberg(sin, 0, pi, tol = 1e-15)    #  2                 , rel.error 1e-15
romberg(exp, 0, 1,  tol = 1e-15)    #  1.718281828459044 , rel error 1e-15
                                    #  1.718281828459045 , i.e. exp(1) - 1

f <- function(x, p) sin(x) * cos(p*x)
romberg(f, 0, pi, p = 2)            #  2/3               , abs.err 1.5e-14
# value: -0.6666667, iter: 7, rel.error: 1e-12
}
\keyword{ math }
