\name{hessian}
\alias{hessian}
\title{
  Hessian Matrix
}
\description{
  Numerically compute the Hessian matrix.
}
\usage{
hessian(f, x0, h = .Machine$double.eps^(1/4), ...)
}
\arguments{
  \item{f}{univariate function of several variables.}
  \item{x0}{point in \eqn{R^n}.}
  \item{h}{step size.}
  \item{...}{variables to be passed to \code{f}.}
}
\details{
  Computes the hessian matrix based on the three-point central difference
  formula, expanded to two variables.

  Assumes that the function has continuous partial derivatives.
}
\value{
  An n-by-n matrix with \eqn{\frac{\partial^2 f}{\partial x_i \partial x_j}}
  as (i, j) entry.
}
\references{
  Fausett, L. V. (2007). Applied Numerical Analysis Using Matlab.
  Second edition, Prentice Hall.
}
\seealso{
  \code{\link{hessdiag}}, \code{\link{hessvec}}, \code{\link{laplacian}}
}
\examples{
f <- function(x) cos(x[1] + x[2])
x0 <- c(0, 0)
hessian(f, x0)

f <- function(u) {
    x <- u[1]; y <- u[2]; z <- u[3]
    return(x^3 + y^2 + z^2 +12*x*y + 2*z)
}
x0 <- c(1,1,1)
hessian(f, x0)
}
\keyword{ math }
