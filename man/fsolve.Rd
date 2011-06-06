\name{fsolve}
\alias{fsolve}
\title{
  Solve System of Nonlinear Equations
}
\description{
  Solve a system of nonlinear equations.
}
\usage{
fsolve(f, x0, ...)
}
\arguments{
  \item{f}{function describing the system of equations.}
  \item{x0}{point near to the root.}
  \item{...}{additional variables to be passed to the function.}
}
\details{
  \code{fsolve} tries to solve the components of function \code{f}
  simultaneously and uses the Gauss-Newton method with numerical gradient
  and Jacobian.

  This function has not yet been implemented and thus stops with an error.
}
\value{
  List with
  \item{x}{location of the solution.}
  \item{fval}{function value at the solution.}
}
\references{
  Antoniou, A., and W.-S. Lu (2007). Practical Optimization: Algorithms and
  Engineering Applications. Springer Science+Business Media, New York.
}
\note{
  \code{fsolve} mimics the Matlab function of the same name.
}
\seealso{
  \code{\link{newtonsys}}
}
\examples{
\dontrun{
# Find a matrix X such that X * X * X = [1, 2; 3, 4]
  F <- function(x) {
    a <- matrix(c(1, 3, 2, 4), nrow = 2, ncol = 2, byrow = TRUE)
    X <- matrix(x,             nrow = 2, ncol = 2, byrow = TRUE)
    return(c(X %*% X %*% X - a))
  }
  x0 <- matrix(1, nrow = 2, ncol = 2)
  fsolve(F, x0)
  # $x                     # newtonsys:
  # -0.1291489  0.8602157
  #  1.2903236  1.1611747
  # $fval
  # 8.881784e-16
}
}
\keyword{ optimize }
