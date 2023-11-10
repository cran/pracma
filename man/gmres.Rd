\name{gmres}
\alias{gmres}
\title{
  Generalized Minimal Residual Method
}
\description{
  \code{gmres(A,b)} attempts to solve the system of linear equations
  \code{A*x=b} for \code{x}.
}
\usage{
    gmres(A, b, x0 = rep(0, length(b)), 
          errtol = 1e-6, kmax = length(b)+1, reorth = 1)
}
\arguments{
  \item{A}{square matrix.}
  \item{b}{numerical vector or column vector.}
  \item{x0}{initial iterate.}
  \item{errtol}{relative residual reduction factor.}
  \item{kmax}{maximum number of iterations}
  \item{reorth}{reorthogonalization method, see Details.}
}
\details{
  Iterative method for the numerical solution of a system of linear equations. 
  The method approximates the solution by the vector in a Krylov subspace with 
  minimal residual. The Arnoldi iteration is used to find this vector.

  Reorthogonalization method:\cr
  1 -- Brown/Hindmarsh condition (default)\cr
  2 -- Never reorthogonalize (not recommended)\cr
  3 -- Always reorthogonalize (not cheap!)
}
\value{
  Returns a list with components \code{x} the solution, \code{error} the 
  vector of residual norms, and \code{niter} the number of iterations.
}
\references{
  C. T. Kelley (1995). Iterative Methods for Linear and Nonlinear Equations.
  SIAM, Society for Industrial and Applied Mathematics, Philadelphia, USA.
}
\author{
  Based on Matlab code from C. T. Kelley's book, see references.
}
\seealso{
  \code{\link{solve}}
}
\examples{
A <- matrix(c(0.46, 0.60, 0.74, 0.61, 0.85,
              0.56, 0.31, 0.80, 0.94, 0.76,
              0.41, 0.19, 0.15, 0.33, 0.06,
              0.03, 0.92, 0.15, 0.56, 0.08,
              0.09, 0.06, 0.69, 0.42, 0.96), 5, 5)
x <- c(0.1, 0.3, 0.5, 0.7, 0.9)
b <- A \%*\% x
gmres(A, b)
# $x
#      [,1]
# [1,]  0.1
# [2,]  0.3
# [3,]  0.5
# [4,]  0.7
# [5,]  0.9
# 
# $error
# [1] 2.37446e+00 1.49173e-01 1.22147e-01 1.39901e-02 1.37817e-02 2.81713e-31
# 
# $niter
# [1] 5
}

\keyword{ math }
