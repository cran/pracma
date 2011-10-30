\name{bisect}
\alias{bisect}
\alias{regulaFalsi}
\title{
Rootfinding Through Bisection
}
\description{
Finding roots of univariate functions in bounded intervals.
}
\usage{
bisect(f, a, b, maxiter = 100, tol = .Machine$double.eps^0.5)

regulaFalsi(f, a, b, maxiter = 100, tol = .Machine$double.eps^0.5)
}
\arguments{
  \item{f}{Function or its name as a string.}
  \item{a, b}{interval end points.}
  \item{maxiter}{maximum number of iterations; default 100.}
  \item{tol}{absolute tolerance; default \code{eps^(1/2)}}
}
\details{
  Well known root finding algorithms for real, univariate, continuous
  functions. Bisection works in any case if the function has opposite
  signs at the endpoints of the interval.

  ``Regula falsi'' combines bisection and secant methods. The so-called
  `Illinois' improvement is used.
}
\value{
  Return a list with components \code{root}, \code{f.root}, 
  the function value at the found root, \code{iter}, the number of iterations
  done, and \code{root}, and the estimated accuracy \code{estim.prec}
}
\references{
  Quarteroni, A., R. Sacco, and F. Saleri (2007). Numerical Mathematics.
  Second Edition, Springer-Verlag, Berlin Heidelberg.
}
\author{
  Hans W Borchers  <hwborchers@googlemail.com>
}
\seealso{
\code{\link{uniroot}}
}
\examples{
# Legendre polynomial of degree 5
lp5 <- c(63, 0, -70, 0, 15, 0)/8
f <- function(x) polyval(lp5, x)
bisect(f, 0.6, 1)       # 0.9061798453 correct to  9 decimals
regulaFalsi(f, 0.6, 1)  # 0.9061798459 correct to 10 decimals
}
\keyword{ math }
