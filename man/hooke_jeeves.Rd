\name{hooke_jeeves}
\alias{hooke_jeeves}
\title{
  Hooke-Jeeves Method
}
\description{
  An implementation of the Hooke-Jeeves algorithm for derivative-free
  optimization.
}
\usage{
hooke_jeeves(x0, f, h = 1, scale = 1,
             info = FALSE, tol = .Machine$double.eps^(2/3), ...)
}
\arguments{
  \item{x0}{start value.}
  \item{f}{function to be minimized.}
  \item{h}{starting step size.}
  \item{scale}{scale factor, of -1 the maximum will be searched for.}
  \item{info}{logical, whether to print information during the main loop.}
  \item{tol}{relative tolerance, to be used as stopping rule.}
  \item{\ldots}{additional arguments to be passed to the function.}
}
\details{
  This method computes a new point using the values of \code{f} at suitable
  points along the orthogonal coordinate directions around the last point.
}
\value{
  List with following components:
    \item{xmin}{minimum solution found.}
    \item{fmin}{value of \code{f} at minimum.}
    \item{fcalls}{number of function calls.}
    \item{niter}{number of iterations performed.}
}
\references{
  Quarteroni, Sacco, and Saleri (2007), Numerical Mathematics, Springer.
}
\note{
  Hooke-Jeeves is notorious for its number of function calls. Memoization
  is often suggested as a remedy.

  For a more elaborate implementation of Hooke-Jeeves see the package
  `dfoptim'.
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
hooke_jeeves(c(0, 0), rosenbrock)
# $xmin
# [1] 1 1
# $fmin
# [1] 1.328283e-16
# $fcalls
# [1] 31934
# $niter
# [1] 4344
}
\keyword{ optimize }
