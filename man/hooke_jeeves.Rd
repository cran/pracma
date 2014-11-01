\name{hooke_jeeves}
\alias{hooke_jeeves}
\title{
  Hooke-Jeeves Minimization Method
}
\description{
  An implementation of the Hooke-Jeeves algorithm for derivative-free
  optimization.
}
\usage{
hooke_jeeves(x0, f, lb = NULL, ub = NULL, tol = 1e-08,
              target = Inf, maxfeval = Inf, info = FALSE, ...)
}
\arguments{
  \item{x0}{starting vector.}
  \item{f}{nonlinear function to be minimized.}
  \item{lb, ub}{lower and upper bounds.}
  \item{tol}{relative tolerance, to be used as stopping rule.}
  \item{target}{iteration stops when this value is reached.}
  \item{maxfeval}{maximum number of allowed function evaluations.}
  \item{info}{logical, whether to print information during the main loop.}
  \item{\ldots}{additional arguments to be passed to the function.}
}
\details{
  This method computes a new point using the values of \code{f} at suitable
  points along the orthogonal coordinate directions around the last point.
}
\value{
  List with following components:
    \item{xmin}{minimum solution found so far.}
    \item{fmin}{value of \code{f} at minimum.}
    \item{fcalls}{number of function evaluations.}
    \item{niter}{number of iterations performed.}
}
\references{
  C.T. Kelley (1999), Iterative Methods for Optimization, SIAM.

  Quarteroni, Sacco, and Saleri (2007), Numerical Mathematics, Springer.
}
\note{
  Hooke-Jeeves is notorious for its number of function calls.
  Memoization is often suggested as a remedy.

  For a similar implementation of Hooke-Jeeves see the `dfoptim' package.
}
\seealso{
  \code{\link{nelder_mead}}
}
\examples{
##  Rosenbrock function
rosenbrock <- function(x) {
    n <- length(x)
    x1 <- x[2:n]
    x2 <- x[1:(n-1)]
    sum(100*(x1-x2^2)^2 + (1-x2)^2)
}

hooke_jeeves(c(0,0,0,0), rosenbrock)
# $xmin
# [1] 1.000000 1.000001 1.000002 1.000004
# $fmin
# [1] 4.774847e-12
# $fcalls
# [1] 2499
# $niter
#[1] 26

hooke_jeeves(rep(0,4), lb=rep(-1,4), ub=0.5, rosenbrock)
# $xmin
# [1] 0.50000000 0.26221320 0.07797602 0.00608027
# $fmin
# [1] 1.667875
# $fcalls
# [1] 571
# $niter
# [1] 26
}
\keyword{ optimize }
