\name{fsolve}
\alias{fsolve}
\title{
  Solve System of Nonlinear Equations
}
\description{
  Solve a system of \code{m} nonlinear equations of \code{n} variables.
}
\usage{
fsolve(f, x0, J = NULL,
       maxiter = 100, tol = .Machine$double.eps^(0.5), ...)
}
\arguments{
  \item{f}{function describing the system of equations.}
  \item{x0}{point near to the root.}
  \item{J}{Jacobian function of \code{f}, or \code{NULL}.}
  \item{maxiter}{maximum number of iterations in \code{gaussNewton}.}
  \item{tol}{tolerance to be used in Gauss-Newton.}
  \item{...}{additional variables to be passed to the function.}
}
\details{
  \code{fsolve} tries to solve the components of function \code{f}
  simultaneously and uses the Gauss-Newton method with numerical gradient
  and Jacobian. If \code{m = n}, it uses \code{broyden}. Not applicable
  for univariate root finding.
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
  \code{\link{broyden}}, \code{\link{gaussNewton}}
}
\examples{
\dontrun{
# Find a matrix X such that X * X * X = [1, 2; 3, 4]
  F <- function(x) {
    a <- matrix(c(1, 3, 2, 4), nrow = 2, ncol = 2, byrow = TRUE)
    X <- matrix(x,             nrow = 2, ncol = 2, byrow = TRUE)
    return(c(X \%*\% X \%*\% X - a))
  }
  x0 <- matrix(1, 2, 2)
  X  <- matrix(fsolve(F, x0)$x, 2, 2)
  X
  # -0.1291489  0.8602157
  #  1.2903236  1.1611747
}
}
\keyword{ optimize }
