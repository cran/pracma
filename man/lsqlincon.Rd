\name{lsqlincon}
\alias{lsqlincon}
\title{
  Linear Least-Squares Fitting with linear constraints
}
\description{
  Solves linearly constrained linear least-squares problems.
}
\usage{
lsqlincon(C, d,  A = NULL, b = NULL,
          Aeq = NULL, beq = NULL, lb = NULL,  ub = NULL)
}
\arguments{
  \item{C}{\code{mxn}-matrix defining the least-squares problem.}
  \item{d}{vector or a one colum matrix with \code{m} rows}
  \item{A}{\code{pxn}-matrix for the linear inequality constraints.}
  \item{b}{vector or \code{px1}-matrix, right hand side for the constraints.}
  \item{Aeq}{\code{qxn}-matrix for the linear equality constraints.}
  \item{beq}{vector or \code{qx1}-matrix, right hand side for the constraints.}
  \item{lb}{lower bounds, a scalar will be extended to length n.}
  \item{ub}{upper bounds, a scalar will be extended to length n.}
}
\details{
  \code{lsqlincon(C, d, A, b, Aeq, beq, lb, ub)} minimizes \code{||C*x - d||}
  (i.e., in the least-squares sense) subject to the following constraints:
  \code{A*x <= b}, \code{Aeq*x = beq}, and \code{lb <= x <= ub}.

  It applies the quadratic solver in \code{quadprog} with an active-set
  method for solving quadratic programming problems.

  If some constraints are \code{NULL} (the default), they will not be taken
  into account. In case no constraints are given at all, it simply uses
  \code{qr.solve}.
}
\value{
  Returns the least-squares solution as a vector.
}
\note{
  Function \code{lsqlin} in \code{pracma} solves this for equality constraints
  only, by computing a base for the nullspace of \code{Aeq}. But for linear
  inequality constraints there is no simple linear algebra `trick', thus a real
  optimization solver is needed.
}
\author{
  HwB  email: <hwborchers@googlemail.com>
}
\references{
  Trefethen, L. N., and D. Bau III. (1997). Numerical Linear Algebra.
  SIAM, Society for Industrial and Applied Mathematics, Philadelphia.
}
\seealso{
  \code{\link{lsqlin}}, \code{quadprog::solve.QP}
}
\examples{
##  MATLABs lsqlin example
C <- matrix(c(
    0.9501,   0.7620,   0.6153,   0.4057,
    0.2311,   0.4564,   0.7919,   0.9354,
    0.6068,   0.0185,   0.9218,   0.9169,
    0.4859,   0.8214,   0.7382,   0.4102,
    0.8912,   0.4447,   0.1762,   0.8936), 5, 4, byrow=TRUE)
d <- c(0.0578, 0.3528, 0.8131, 0.0098, 0.1388)
A <- matrix(c(
    0.2027,   0.2721,   0.7467,   0.4659,
    0.1987,   0.1988,   0.4450,   0.4186,
    0.6037,   0.0152,   0.9318,   0.8462), 3, 4, byrow=TRUE)
b <- c(0.5251, 0.2026, 0.6721)
Aeq <- matrix(c(3, 5, 7, 9), 1)
beq <- 4
lb <- rep(-0.1, 4)   # lower and upper bounds
ub <- rep( 2.0, 4)

x <- lsqlincon(C, d, A, b, Aeq, beq, lb, ub)
# -0.1000000 -0.1000000  0.1599088  0.4089598
# check A %*% x - b >= 0
# check Aeq %*% x - beq == 0
# check sum((C %*% x - d)^2)    # 0.1695104
}
\keyword{ optimize }
