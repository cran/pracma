\name{steep_descent}
\alias{steep_descent}
\title{
  Steepest Descent Minimization
}
\description{
  Function minimization by steepest descent.
}
\usage{
steep_descent(x0, f, g = NULL, info = FALSE,
              maxiter = 100, tol = .Machine$double.eps^(1/2))
}
\arguments{
  \item{x0}{start value.}
  \item{f}{function to be minimized.}
  \item{g}{gradient function of \code{f};
           if \code{NULL}, a numerical gradient will be calculated.}
  \item{info}{logical; shall information be printed on every iteration?}
  \item{maxiter}{max. number of iterations.}
  \item{tol}{relative tolerance, to be used as stopping rule.}
}
\details{
  Steepest descent is a line search method that moves along the downhill
  direction.
}
\value{
  List with following components:
    \item{xmin}{minimum solution found.}
    \item{fmin}{value of \code{f} at minimum.}
    \item{niter}{number of iterations performed.}
}
\references{
  Nocedal, J., and S. J. Wright (2006). Numerical Optimization.
  Second Edition, Springer-Verlag, New York, pp. 22 ff.
}
\note{
  Used some Matlab code as described in the book ``Applied Numerical Analysis
  Using Matlab'' by L. V.Fausett.
}
\seealso{
  \code{\link{fletcher_powell}}
}
\examples{
##  Rosenbrock function: The flat valley of the Rosenbruck function makes
##  it infeasible for a steepest descent approach.
# rosenbrock <- function(x) {
#     n <- length(x)
#     x1 <- x[2:n]
#     x2 <- x[1:(n-1)]
#     sum(100*(x1-x2^2)^2 + (1-x2)^2)
# }
# steep_descent(c(1, 1), rosenbrock)
# Warning message:
# In steep_descent(c(0, 0), rosenbrock) :
#   Maximum number of iterations reached -- not converged.

## Sphere function
sph <- function(x) sum(x^2)
steep_descent(rep(1, 10), sph)
# $xmin   0 0 0 0 0 0 0 0 0 0
# $fmin   0
# $niter  2
}
\keyword{ optimize }
