\name{anms}
\alias{anms}
\title{
  Adaptive Nelder-Mead Minimization
}
\description{
  An implementation of the Nelder-Mead algorithm for derivative-free
  optimization / function minimization.
}
\usage{
anms(fn, x0, ...,
     tol = 1e-10, maxfeval = NULL)
}
\arguments{
  \item{fn}{nonlinear function to be minimized.}
  \item{x0}{starting vector.}
  \item{tol}{relative tolerance, to be used as stopping rule.}
  \item{maxfeval}{maximum number of function calls.}
  \item{\ldots}{additional arguments to be passed to the function.}
}
\details{
  Also called a `simplex' method for finding the local minimum of a function
  of several variables. The method is a pattern search that compares function
  values at the vertices of the simplex. The process generates a sequence of
  simplices with ever reducing sizes.

  \code{anms} can be used up to 20 or 30 dimensions (then `tol' and `maxfeval'
  need to be increased). It applies adaptive parameters for simplicial search, 
  depending on the problem dimension -- see Fuchang and Lixing (2012).

  With upper and/or lower bounds, \code{anms} will apply a transformation of
  bounded to unbounded regions before utilizing Nelder-Mead. Of course, if the
  optimum is near to the boundary, results will not be as accurate as when the
  minimum is in the interior.
}
\value{
  List with following components:
    \item{xmin}{minimum solution found.}
    \item{fmin}{value of \code{f} at minimum.}
    \item{nfeval}{number of function calls performed.}
}
\references{
  Nelder, J., and R. Mead (1965). A simplex method for function minimization.
  Computer Journal, Volume 7, pp. 308-313.

  O'Neill, R. (1971). Algorithm AS 47: Function Minimization Using a Simplex 
  Procedure. Applied Statistics, Volume 20(3), pp. 338-345.

  J. C. Lagarias et al. (1998). Convergence properties of the Nelder-Mead
  simplex method in low dimensions. SIAM Journal for Optimization, Vol. 9,
  No. 1, pp 112-147.

  Fuchang Gao and Lixing Han (2012). Implementing the Nelder-Mead simplex
  algorithm with adaptive parameters. Computational Optimization and
  Applications, Vol. 51, No. 1, pp. 259-277.
}
\note{
  Copyright (c) 2012 by F. Gao and L. Han, implemented in Matlab with a
  permissive license. Implemented in R by Hans W. Borchers. For another
  elaborate implementation of Nelder-Mead see the package `dfoptim'.
}
\seealso{
  \code{\link{optim}}
}
\examples{
##  Rosenbrock function
rosenbrock <- function(x) {
    n <- length(x)
    x1 <- x[2:n]
    x2 <- x[1:(n-1)]
    sum(100*(x1-x2^2)^2 + (1-x2)^2)
}

anms(rosenbrock, c(0,0,0,0,0))
# $xmin
# [1] 1 1 1 1 1
# $fmin
# [1] 8.268732e-21
# $nfeval
# [1] 1153

# To add constraints to the optimization problem, use a slightly 
# modified objective function. Equality constraints not possible.
# Warning: Avoid a starting value too near to the boundary !

\dontrun{
# Example: 0.0 <= x <= 0.5
fun <- function(x) {
    if (any(x < 0) || any(x > 0.5)) 100
    else rosenbrock(x)
}
x0 <- rep(0.1, 5)

anms(fun, x0)
## $xmin
## [1] 0.500000000 0.263051265 0.079972922 0.016228138 0.000267922}
}
\keyword{ optimize }
