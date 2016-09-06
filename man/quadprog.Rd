\name{quadprog}
\alias{quadprog}
\title{
  Quadratic Programming
}
\description{
  Solves quadratic programming problems with linear and box constraints.
}
\usage{
quadprog(C, d, A = NULL, b = NULL,
         Aeq = NULL, beq = NULL, lb = NULL, ub = NULL) 
}
\arguments{
  \item{C}{symmetric matrix, representing the quadratic term.}
  \item{d}{vector, representing the linear term.}
  \item{A}{matrix, represents the linear constraint coefficients.}
  \item{b}{vector, constant vector in the constraints.}
  \item{Aeq}{matrix, linear equality constraint coefficients.}
  \item{beq}{vector, constant equality constraint vector.}
  \item{lb}{elementwise lower bounds.}
  \item{ub}{elementwise upper bounds.}
}
\details{
  Finds a minimum for the quadratic programming problem specified as:
  \deqn{min 1/2 x'Cx + d'x}
  such that the following constraints are satisfied:
  \deqn{A x <= b}
  \deqn{Aeq x = beq}
  \deqn{lb <= x <= ub}
  The matrix should be symmetric and positive definite, in which case
  the solution is unique, indicated when the exit flag is 1.

  For more information, see \code{?solve.QP}.
}
\value{
  Returns a list with components
    \item{xmin}{minimum solution, subject to all bounds and constraints.}
    \item{fval}{value of the target expression at the arg minimum.}
    \item{eflag}{exit flag.}
}
\references{
  Nocedal, J., and St. J. Wright (2006). Numerical Optimization.
  Second Edition, Springer Series in Operations Research, New York.
}
\note{
  This function is wrapping the active set quadratic solver in the 
  \code{quadprog} package: \code{quadprog::solve.QP}, combined with
  a more MATLAB-like API interface.
}
\seealso{
  \code{\link{lsqlincon}}, \code{quadprog::solve.QP}
}
\examples{
## Example in ?solve.QP
# Assume we want to minimize: 1/2 x^T x - (0 5 0) %*% x
# under the constraints:      A x <= b
# with b = (8,-2, 0)
# and      ( 4  3  0) 
#      A = (-2 -1  0)
#          ( 0  2,-1)
# and possibly equality constraint  3x1 + 2x2 + x3 = 1
# or upper bound c(1.5, 1.5, 1.5).

C <- diag(1, 3); d <- -c(0, 5, 0)
A <- matrix(c(4,3,0, -2,-1,0, 0,2,-1), 3, 3, byrow=TRUE)
b <- c(8, -2, 0)

quadprog(C, d, A, b)
# $xmin
# [1] 0.4761905 1.0476190 2.0952381
# $fval
# [1] -2.380952
# $eflag
# [1] 1

Aeq <- c(3, 2, 1);  beq <- 1
quadprog(C, d, A, b, Aeq, beq)
# $xmin
# [1]  1.4 -0.8 -1.6
# $fval
# [1] 6.58
# $eflag
# [1] 1

quadprog(C, d, A, b, lb = 0, ub = 1.5)
# $xmin
# [1] 0.625 0.750 1.500
# $fval
# [1] -2.148438
# $eflag
# [1] 1

## Example help(quadprog)
C <- matrix(c(1, -1, -1, 2), 2, 2)
d <- c(-2, -6)
A <- matrix(c(1,1, -1,2, 2,1), 3, 2, byrow=TRUE)
b <- c(2, 2, 3)
lb <- c(0, 0)

quadprog(C, d, A, b, lb=lb)
# $xmin
# [1] 0.6666667 1.3333333
# $fval
# [1] -8.222222
# $eflag
# [1] 1
}
\keyword{ optimize }
