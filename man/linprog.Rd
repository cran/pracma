\name{linprog}
\alias{linprog}
\title{
  Linear Programming Solver
}
\description{
  Solves simple linear programming problems, allowing for inequality and
  equality constraints as well as lower and upper bounds.
}
\usage{
linprog(cc, A = NULL, b = NULL, Aeq = NULL, beq = NULL,
        lb = NULL, ub = NULL, x0 = NULL, I0 = NULL,
        bigM = 100, maxiter = 20, maximize = FALSE)
}
\arguments{
  \item{cc}{defines the linear objective function.}
  \item{A}{matrix representing the inequality constraints \code{A x <= b}.}
  \item{b}{vector, right hand side of the inequalities.}
  \item{Aeq}{matrix representing the equality constraints \code{Aeq x <= beq}.}
  \item{beq}{vector, right hand side of the inequalities.}
  \item{lb}{lower bounds, if not \code{NULL} must all be greater or equal 0.}
  \item{ub}{upper bounds, if not \code{NULL} must all be greater or equal \code{lb}.}
  \item{x0}{feasible base vector, will not be used at the moment.}
  \item{I0}{index set of \code{x0}, will not be used at the moment.}
  \item{bigM}{big-M constant, will be used for finding a base vector.}
  \item{maxiter}{maximum number of iterations.}
  \item{maximize}{logical; shall the objective be minimized or maximized?}
}
\details{
  Solves linear programming problems of the form \eqn{min cc' * x}
  such that
  \deqn{A * x \le b}
  \deqn{A_{eq} * x = b_{eq}}
  \deqn{lb \le x \le ub}
}
\value{
  List with
  \itemize{
    \item{\code{x}} the solution vector.
    \item{\code{fval}} the value at the optimal solution.
    \item{\code{errno}, \code{mesage}} the error number and message.
  }
}
\references{
  Vanderbei, R. J. (2001). Linear Programming: Foundations and Extensions.
  Princeton University Press.

  Eiselt, H. A., and C.-L. Sandblom (2012). Operations Research: 
  A Model-based Approach. Springer-Verlag, Berlin Heidelberg.
}
\author{
  HwB <hwborchers@googlemail.com>
}
\note{
  This is a first version that will be unstable at times. For real linear
  programming problems use package \code{lpSolve}.
}
\seealso{
  \code{linprog::solveLP}, \code{lpSolve::lp}
}
\examples{
##  Examples from the book "Operations research - A Model-based Approach"
#-- production planning
cc <- c(5, 3.5, 4.5)
Ain <- matrix(c(3, 5, 4,
                6, 1, 3), 2, 3, byrow=TRUE)
bin <- c(540, 480)
linprog(cc, A = Ain, b = bin, maximize = TRUE)
# $x     20   0 120
# $fval  640

#-- diet problem
cc <- c(1.59, 2.19, 2.99)
Ain <- matrix(c(-250, -380, -257,
                 250,  380,  257,
                  13,   31,   28), 3, 3, byrow = TRUE)
bin <- c(-1800, 2200, 100)
linprog(cc, A = Ain, b = bin)

#-- employee scheduling
cc <- c(1, 1, 1, 1, 1, 1)
A <- (-1)*matrix(c(1, 0, 0, 0, 0, 1,
                   1, 1, 0, 0, 0, 0,
                   0, 1, 1, 0, 0, 0,
                   0, 0, 1, 1, 0, 0,
                   0, 0, 0, 1, 1, 0,
                   0, 0, 0, 0, 1, 1), 6, 6, byrow = TRUE)
b <- -c(17, 9, 19, 12, 5, 8)
linprog(cc, A, b)

#-- inventory models
cc <- c(1, 1.1, 1.2, 1.25, 0.05, 0.15, 0.15)
Aeq <- matrix(c(1, 0, 0, 0, -1,  0,  0,
                0, 1, 0, 0,  1, -1,  0,
                0, 0, 1, 0,  0,  1, -1,
                0, 0, 0, 1,  0,  0,  1), 4, 7, byrow = TRUE)
beq <- c(60, 70, 130, 150)
ub <- c(120, 140, 150, 140, Inf, Inf, Inf)
linprog(cc, Aeq = Aeq, beq = beq, ub = ub)

#-- allocation problem
cc <- c(1, 1, 1, 1, 1)
A <- matrix(c(-5,    0,    0,    0,    0,
               0, -4.5,    0,    0,    0,
               0,    0, -5.5,    0,    0,
               0,    0,    0, -3.5,    0,
               0,    0,    0,    0, -5.5,
               5,    0,    0,    0,    0,
               0,  4.5,    0,    0,    0,
               0,    0,  5.5,    0,    0,
               0,    0,    0,  3.5,    0,
               0,    0,    0,    0,  5.5,
              -5, -4.5, -5.5, -3.5, -5.5,
              10, 10.0, 10.0, 10.0, 10.0,
              0.2, 0.2,  0.2, -1.0,  0.2), 13, 5, byrow = TRUE)
b <- c(-50, -55, -60, -50, -50, rep(100, 5), -5*64, 700, 0)
# linprog(cc, A = A, b = b)
lb <- b[1:5] / diag(A[1:5, ])
ub <- b[6:10] / diag(A[6:10, ])
A1 <- A[11:13, ]
b1 <- b[11:13]
linprog(cc, A1, b1, lb = lb, ub = ub)

#-- transportation problem
cc <- c(1, 7, 4, 2, 3, 5)
Aeq <- matrix(c(1, 1, 1, 0, 0, 0,
                0, 0, 0, 1, 1, 1,
                1, 0, 0, 1, 0, 0,
                0, 1, 0, 0, 1, 0,
                0, 0, 1, 0, 0, 1), 5, 6, byrow = TRUE)
beq <- c(30, 20, 15, 25, 10)
linprog(cc, Aeq = Aeq, beq = beq)
}
\keyword{ optimize }
