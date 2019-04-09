\name{qpspecial, qpsolve}
\alias{qpspecial}
\alias{qpsolve}
\title{
  Special Quadratic Programming Solver
}
\description{
  Solves a special Quadratic Programming problem.
}
\usage{
qpspecial(G, x, maxit = 100)

qpsolve(d, A, b, meq = 0, tol = 1e-07)
}
\arguments{
  \item{G}{\code{m x n}-matrix.}
  \item{x}{column vector of length \code{n}, the initial (feasible) iterate; 
           if not present (or requirements on x0 not met), x0 will be found.}
  \item{maxit}{maximum number of iterates allowed; default 100.}
  \item{d}{Linear term of the quadratic form.}
  \item{A, b}{Linear equality and inequality constraints.}
  \item{meq}{First meq rows are used as equality constraints.}
  \item{tol}{Tolerance used for stopping the iteration.}
}
\details{
  \code{qpspecial} solves the special QP problem:

  \code{min     q(x)  = || G*x ||_2^2 = x'*(G'*G)*x}\cr
  \code{s.t.  sum(x)  = 1}\cr
  \code{and   x  >= 0}

  The problem corresponds to finding the smallest vector (2-norm) in the 
  convex hull of the columns of \code{G}.
  
  \code{qpsolve} solves the more general QP problem:

  \code{min  q(x) = 0.5 t(x)*x - d x}\cr
  \code{s.t. A x >= b}

  with \code{A x = b} for the first \code{meq} rows.
}
\value{
  Returns a list with the following components:
  \itemize{
  \item \code{x} -- optimal point attaining optimal value;
  \item \code{d = G*x} -- smallest vector in the convex hull;
  \item \code{q} -- optimal value found, \code{= t(d) \%*\% d};
  \item \code{niter} -- number of iterations used;
  \item \code{info} -- error number:\cr
    \code{= 0}: everything went well, q is optimal,\cr
    \code{= 1}:  maxit reached and final x is feasible,\cr
    \code{= 2}:  something went wrong.
  }
}
\note{
  \code{x} may be missing, same as if requirements are not met; may stop with 
  an error if \code{x} is not feasible.
}
\references{
  [Has to be found.]
}
\author{
  Matlab code by Anders Skajaa, 2010, under GPL license (HANSO toolbox);
  converted to R by Abhirup Mallik and Hans W. Borchers, with permission.
}
\examples{
G <- matrix(c(0.31, 0.99, 0.54, 0.20,
              0.56, 0.97, 0.40, 0.38,
              0.81, 0.06, 0.44, 0.80), 3, 4, byrow =TRUE)
qpspecial(G)
# $x
#              [,1]
# [1,] 1.383697e-07
# [2,] 5.221698e-09
# [3,] 8.648168e-01
# [4,] 1.351831e-01
# $d
#           [,1]
# [1,] 0.4940377
# [2,] 0.3972964
# [3,] 0.4886660
# $q
# [1] 0.6407121
# $niter
# [1] 6
# $info
# [1] 0

# Example from quadprog::solve.QP
d <- c(0,5,0)
A <- matrix(c(-4,-3,0,2,1,0,0,-2,1),3,3)
b <- c(-8,2,0)
qpsolve(d, A, b)
## $sol
## [1] 0.4761905 1.0476190 2.0952381
## $val
## [1] -2.380952
## $niter
## [1] 3
}
