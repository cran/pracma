\name{arclength}
\alias{arclength}
\title{
  Arc Length of a Curve
}
\description{
  Calculates the arc length of a parametrized curve.
}
\usage{
arclength(f, a, b, nmax = 20, tol = 1e-05, ...)
}
\arguments{
  \item{f}{parametrization of a curve in n-dim. space.}
  \item{a,b}{begin and end of the parameter interval.}
  \item{nmax}{maximal number of iterations.}
  \item{tol}{relative tolerance requested.}
  \item{...}{additional arguments to be passed to the function.}
}
\details{
  Calculates the arc length of a parametrized curve in \code{R^n}. It applies
  Richardson's extrapolation by refining polygon approximations to the curve.

  The parametrization of the curve must be vectorized:
  if \code{t-->F(t)} is the parametrization, \code{F(c(t1,t1,...))} must
  return \code{c(F(t1),F(t2),...)}.

  Can be directly applied to determine the arc length of a one-dimensional 
  function \code{f:R-->R} by defining \code{F} (if \code{f} is vectorized)
  as \code{F:t-->c(t,f(t))}.
}
\value{
  Returns a list with components \code{length} the calculated arc length,
  \code{niter} the number of iterations, and \code{rel.err} the relative
  error generated from the extrapolation.
}
\author{
  HwB <hwborchers@googlemail.com>
}
\note{
  If by chance certain equidistant points of the curve lie on a straight line,
  the result may be wrong, then use \code{polylength} below.
}
\seealso{
  \code{\link{poly_length}}
}
\examples{
##  Example: parametrized 3D-curve with t in 0..3*pi
f <- function(t) c(sin(2*t), cos(t), t)
arclength(f, 0, 3*pi)
# $length:  17.22203            # true length 17.222032...

##  Example: length of the sine curve
f <- function(t) c(t, sin(t))
arclength(f, 0, pi)             # true length  3.82019...

\dontrun{
##  Example: oscillating 1-dimensional function (from 0 to 5)
f <- function(x) x * cos(0.1*exp(x)) * sin(0.1*pi*exp(x))
F <- function(t) c(t, f(t))
L <- arclength(F, 0, 5, tol = 1e-12, nmax = 25)
print(L$length, digits = 16)
# [1] 82.81020372882217         # true length 82.810203728822172...

# Split this computation in 10 steps (run time drops from 2 to 0.2 secs)
L <- 0
for (i in 1:10)
	L <- L + arclength(F, (i-1)*0.5, i*0.5, tol = 1e-10)$length
print(L, digits = 16)
# [1] 82.81020372882216

# Alternative calculation of arc length
f1 <- function(x) sqrt(1 + complexstep(f, x)^2)
L1 <- quadgk(f1, 0, 5, tol = 1e-14)
print(L1, digits = 16)
# [1] 82.81020372882216
  }
}
\keyword{ math }
