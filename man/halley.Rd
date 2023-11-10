\name{halley}
\alias{halley}

\title{
  Halley's Root Finding Mathod
}
\description{
  Finding roots of univariate functions using the Halley method.
}
\usage{
halley(fun, x0, maxiter = 500, tol = 1e-08, ...)
}
\arguments{
  \item{fun}{function whose root is to be found.}
  \item{x0}{starting value for the iteration.}
  \item{maxiter}{maximum number of iterations.}
  \item{tol}{absolute tolerance; default \code{eps^(1/2)}}
  \item{...}{additional arguments to be passed to the function.}
}
\details{
  Well known root finding algorithms for real, univariate, continuous
  functions; the second derivative must be smooth, i.e. continuous.
  The first and second derivative are computed numerically.
}
\value{
  Return a list with components \code{root}, \code{f.root}, 
  the function value at the found root, \code{iter}, the number of iterations
  done, and the estimated precision \code{estim.prec}
}
\references{
  \url{https://mathworld.wolfram.com/HalleysMethod.html}
}
\seealso{
\code{\link{newtonRaphson}}
}
\examples{
halley(sin, 3.0)        # 3.14159265358979 in 3 iterations
halley(function(x) x*exp(x) - 1, 1.0)
                        # 0.567143290409784 Gauss' omega constant

# Legendre polynomial of degree 5
lp5 <- c(63, 0, -70, 0, 15, 0)/8
f <- function(x) polyval(lp5, x)
halley(f, 1.0)          # 0.906179845938664
}
\keyword{ math }
