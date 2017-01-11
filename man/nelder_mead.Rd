\name{nelder_mead}
\alias{nelder_mead}
\title{
  Nelder-Mead Minimization
}
\description{
  An implementation of the Nelder-Mead algorithm for derivative-free
  optimization / function minimization.
}
\usage{
nelder_mead(x0, f, lb = NULL, ub = NULL, tol = 1e-10,
            maxfeval = 20000, step = rep(1.0, length(x0)), ...)
}
\arguments{
  \item{x0}{starting vector.}
  \item{f}{nonlinear function to be minimized.}
  \item{lb, ub}{lower and upper of a bounded region.}
  \item{tol}{relative tolerance, to be used as stopping rule.}
  \item{maxfeval}{maximum number of function calls.}
  \item{step}{size and shape of initial simplex; relative magnitudes of
              its elements should reflect the units of the variables.}
  \item{\ldots}{additional arguments to be passed to the function.}
}
\details{
  Also called a `simplex' method for finding the local minimum of a function
  of several variables. The method is a pattern search that compares function
  values at the vertices of the simplex. The process generates a sequence of
  simplices with ever reducing sizes.

  `nelder_mead()' can be used up to 20 dimensions (then `tol' and `maxfeval'
  need to be increased). Since version 1.9.8 'nelder_mead()' applies adaptive 
  parameters for the simplicial search, depending on the problem dimension --
  see Fuchang and Lixing (2012).

  With upper and/or lower bounds, `nelder_mead()' applies a transformation of
  bounded to unbounded regions before utilizing Nelder-Mead. Of course, if the
  optimum is near to the boundary, results will not be as accurate as when the
  minimum is in the interior.
}
\value{
  List with following components:
    \item{xmin}{minimum solution found.}
    \item{fmin}{value of \code{f} at minimum.}
    \item{nfeval}{number of iterations performed.}
    \item{restarts}{number of restarts.}
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
  Original FORTRAN77 version by R O'Neill; MATLAB version by John Burkardt
  under LGPL license. Re-implemented in R by Hans W. Borchers. For another
  elaborate implementation of Nelder-Mead see the package `dfoptim'.
}
\seealso{
  \code{\link{hooke_jeeves}}
}
\examples{
##  Rosenbrock function
rosenbrock <- function(x) {
    n <- length(x)
    x1 <- x[2:n]
    x2 <- x[1:(n-1)]
    sum(100*(x1-x2^2)^2 + (1-x2)^2)
}

nelder_mead(c(0,0,0,0), rosenbrock)
# $xmin
# [1] 1 1 1 1
# $fmin
# [1] 8.802801e-25
# $nfeval
# [1] 678
# $restarts
# [1] 0

nelder_mead(c(0,0,0,0), rosenbrock, rep(-0.5, 4), rep(0.5, 4))
# $xmin
# [1] 0.50000000 0.26221321 0.07797600 0.00608026
# $fmin
# [1] 1.667875
# $nfeval
# [1] 1501
# $restarts
# [1] 3
}
\keyword{ optimize }
