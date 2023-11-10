\name{fminbnd}
\alias{fminbnd}
\title{
  Finding Function Minimum
}
\description{
  Find minimum of single-variable function on fixed interval.
}
\usage{
fminbnd(f, a, b, maxiter = 1000, maximum = FALSE,
        tol = 1e-07, rel.tol = tol, abs.tol = 1e-15, ...)
}
\arguments{
  \item{f}{function whose minimum or maximum is to be found.}
  \item{a, b}{endpoints of the interval to be searched.}
  \item{maxiter}{maximal number of iterations.}
  \item{maximum}{logical; shall maximum or minimum be found; default FALSE.}
  \item{tol}{relative tolerance; left over for compatibility.}
  \item{rel.tol, abs.tol}{relative and absolute tolerance.}
  \item{...}{additional variables to be passed to the function.}
}
\details{
  fminbnd finds the minimum of a function of one variable within a fixed
  interval. It applies Brent's algorithm, based on golden section search and
  parabolic interpolation.

  \code{fminbnd} may only give local solutions.
  \code{fminbnd} never evaluates \code{f} at the endpoints.
}
\value{
  List with
  \item{xmin}{location of the minimum resp. maximum.}
  \item{fmin}{function value at the optimum.}
  \item{niter}{number of iterations used.}
  \item{estim.prec}{estimated precision.}
}
\references{
  R. P. Brent (1973). Algorithms for Minimization Without Derivatives.
  Dover Publications, reprinted 2002.
}
\note{
  \code{fminbnd} mimics the Matlab function of the same name.
}
\seealso{
  \code{\link{fibsearch}}, \code{\link{golden_ratio}}
}
\examples{
##  CHEBFUN example by Trefethen
f <- function(x) exp(x)*sin(3*x)*tanh(5*cos(30*x))
fminbnd(f, -1, 1)                   # fourth local minimum (from left)
g <- function(x) complexstep(f, x)  # complex-step derivative
xs <- findzeros(g, -1, 1)           # local minima and maxima
ys <- f(xs); n0 <- which.min(ys)    # index of global minimum
fminbnd(f, xs[n0-1], xs[n0+1])      # xmin:0.7036632, fmin: -1.727377

\dontrun{
ezplot(f, -1, 1, n = 1000, col = "darkblue", lwd = 2)
ezplot(function(x) g(x)/150, -1, 1, n = 1000, col = "darkred", add = TRUE)
grid()}
}
\keyword{ optimize }
