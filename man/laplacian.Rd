\name{laplacian}
\alias{laplacian}
\title{
  Laplacian Operator
}
\description{
  Numerically compute the Laplacian of a function.
}
\usage{
laplacian(f, x0, h = .Machine$double.eps^(1/4), ...)
}
\arguments{
  \item{f}{univariate function of several variables.}
  \item{x0}{point in \eqn{R^n}.}
  \item{h}{step size.}
  \item{...}{variables to be passed to \code{f}.}
}
\details{
  Computes the Laplacian operator \eqn{f_{x_1 x_1} + \ldots + f_{x_n x_n}}
  based on the three-point central difference formula, expanded to this
  special case.

  Assumes that the function has continuous partial derivatives.
}
\value{
  Real number.
}
\references{
  Fausett, L. V. (2007). Applied Numerical Analysis Using Matlab.
  Second edition, Prentice Hall.
}
\seealso{
  \code{\link{hessian}}
}
\examples{
f <- function(x) x[1]^2 + 2*x[1]*x[2] + x[2]^2
laplacian(f, c(1,1))
}
\keyword{ math }
