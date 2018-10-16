\name{bisect}
\alias{bisect}
\alias{secant}
\alias{regulaFalsi}
\title{
Rootfinding Through Bisection or Secant Rule
}
\description{
Finding roots of univariate functions in bounded intervals.
}
\usage{
bisect(fun, a, b, maxiter = 500, tol = NA, ...)

secant(fun, a, b, maxiter = 500, tol = 1e-08, ...)

regulaFalsi(fun, a, b, maxiter = 500, tol = 1e-08, ...)
}
\arguments{
  \item{fun}{Function or its name as a string.}
  \item{a, b}{interval end points.}
  \item{maxiter}{maximum number of iterations; default 100.}
  \item{tol}{absolute tolerance; default \code{eps^(1/2)}}
  \item{...}{additional arguments passed to the function.}
}
\details{
  ``Bisection'' is a well known root finding algorithms for real, univariate, 
  continuous functions. Bisection works in any case if the function has
  opposite signs at the endpoints of the interval.

  \code{bisect} stops when floating point precision is reached, attaching
  a tolerance is no longer needed. This version is trimmed for exactness, 
  not speed. Special care is taken when 0.0 is a root of the function.
  Argument 'tol' is deprecated and not used anymore.

  The ``Secant rule'' uses a succession of roots of secant lines to better 
  approximate a root of a function. ``Regula falsi'' combines bisection and 
  secant methods. The so-called `Illinois' improvement is used here.
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
\seealso{
\code{\link{ridders}}
}
\examples{
bisect(sin, 3.0, 4.0)
# $root             $f.root             $iter   $estim.prec
# 3.1415926536      1.2246467991e-16    52      4.4408920985e-16

bisect(sin, -1.0, 1.0)
# $root             $f.root             $iter   $estim.prec
# 0                 0                   2       0

# Legendre polynomial of degree 5
lp5 <- c(63, 0, -70, 0, 15, 0)/8
f <- function(x) polyval(lp5, x)
bisect(f, 0.6, 1)       # 0.9061798453      correct to 15 decimals
secant(f, 0.6, 1)       # 0.5384693         different root
regulaFalsi(f, 0.6, 1)  # 0.9061798459      correct to 10 decimals
}
\keyword{ math }
