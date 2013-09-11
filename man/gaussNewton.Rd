\name{gaussNewton}
\alias{gaussNewton}
\title{Gauss-Newton Function Minimization}
\description{
  Gauss-Newton method of minimizing a term \eqn{f_1(x)^2 + \ldots + f_m(x)^2}
  or \eqn{F' F} where \eqn{F = (f_1, \ldots, f_m)} is a multivariate function
  of \eqn{n} variables, not necessarily \eqn{n = m}.
}
\usage{
gaussNewton(x0, Ffun, Jfun = NULL,
                        maxiter =100, tol = .Machine$double.eps^(1/2), ...)
}
\arguments{
\item{Ffun}{\code{m} functions of \code{n} variables.}
\item{Jfun}{function returning the Jacobian matrix of \code{Ffun};
            if \code{NULL}, the default, the Jacobian will be computed
            numerically. The gradient of \code{f} will be computed
            internally from the Jacobian (i.e., cannot be supplied).}
\item{x0}{Numeric vector of length \code{n}.}
\item{maxiter}{Maximum number of iterations.}
\item{tol}{Tolerance, relative accuracy.}
\item{...}{Additional parameters to be passed to f.}
}
\details{
  Solves the system of equations applying the Gauss-Newton's method. It is
  especially designed for minimizing a sum-of-squares of functions and can
  be used to find a common zero of several function.
  
  This algorithm is described in detail in the textbook by Antoniou and Lu,
  incl. different ways to modify and remedy the Hessian if not being positive
  definite. Here, the approach by Goldfeld, Quandt and Trotter is used, and 
  the hessian modified by the Matthews and Davies algorithm if still not
  invertible.

  To accelerate the iteration, an inexact linesearch is applied.
}
\value{
  List with components:\cr
  \code{xs} the minimum or root found so far,\cr
  \code{fs} the square root of sum of squares of the values of f,\cr
  \code{iter} the number of iterations needed, and\cr
  \code{relerr} the absoulte distance between the last two solutions.
}
\references{
  Antoniou, A., and W.-S. Lu (2007). Practical Optimization: Algorithms and
  Engineering Applications. Springer Business+Science, New York.
}
\note{
  If \code{n=m} then directly applying the \code{newtonsys} function might
  be a better alternative.
}
\seealso{
  \code{\link{newtonsys}}, \code{\link{softline}}
}
\examples{
f1 <- function(x) c(x[1]^2 + x[2]^2 - 1, x[1] + x[2] - 1)
gaussNewton(c(4, 4), f1)

f2 <- function(x) c( x[1] + 10*x[2], sqrt(5)*(x[] - x[4]),
                    (x[2] - 2*x[3])^2, 10*(x[1] - x[4])^2)
gaussNewton(c(-2, -1, 1, 2), f2)

f3 <- function(x)
        c(2*x[1] - x[2] - exp(-x[1]), -x[1] + 2*x[2] - exp(-x[2]))
gaussNewton(c(0, 0), f3)
# $xs   0.5671433 0.5671433

f4 <- function(x)  # Dennis Schnabel
        c(x[1]^2 + x[2]^2 - 2, exp(x[1] - 1) + x[2]^3 - 2)
gaussNewton(c(2.0, 0.5), f4)
# $xs    1 1

##  Examples (from Matlab)
F1 <- function(x) c(2*x[1]-x[2]-exp(-x[1]), -x[1]+2*x[2]-exp(-x[2]))
gaussNewton(c(-5, -5), F1)

# Find a matrix X such that X %*% X %*% X = [1 2; 3 4]
F2 <- function(x) {
    X <- matrix(x, 2, 2)
    D <- X \%*\% X \%*\% X - matrix(c(1,3,2,4), 2, 2)
    return(c(D))
}
sol <- gaussNewton(ones(2,2), F2)
(X  <- matrix(sol$xs, 2, 2))
#            [,1]      [,2]
# [1,] -0.1291489 0.8602157
# [2,]  1.2903236 1.1611747
X \%*\% X \%*\% X
}
\keyword{ math }
