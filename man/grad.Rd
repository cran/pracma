\name{grad}
\alias{grad}
\title{
  Numerical Gradient
}
\description{
  Numerical function gradient.
}
\usage{
grad(f, x0, heps = .Machine$double.eps^(1/3), ...)
}
\arguments{
  \item{f}{function of several variables.}
  \item{x0}{point where the gradient is to build.}
  \item{heps}{step size.}
  \item{...}{more variables to be passed to function \code{f}.}
}
\details{
  Computes the gradient
  \deqn{(\frac{\partial f}{\partial x_1}, \ldots, \frac{\partial f}{\partial x_n})}
  numerically using the ``central difference formula''.
}
\value{
  Vector of the same length as \code{x0}.
}
\references{
  Mathews, J. H., and K. D. Fink (1999). Numerical Methods Using Matlab.
  Third Edition, Prentice Hall.
}
\seealso{
  \code{\link{fderiv}}
}
\examples{
f <- function(u) {
    x <- u[1]; y <- u[2]; z <- u[3]
    return(x^3 + y^2 + z^2 +12*x*y + 2*z)
 }
x0 <- c(1,1,1)
grad(f, x0)     # 15 14  4        # direction of steepest descent

sum(grad(f, x0) * c(1, -1, 0))    # 1 , directional derivative

f <- function(x) x[1]^2 + x[2]^2
grad(f, c(0,0))                   # 0 0 , i.e. a local optimum
}
\keyword{ math }
