\name{nelder_mead}
\alias{nelder_mead}
\alias{nelmin}
\alias{nelminb}
\title{
  Nelder-Mead Minimization Method
}
\description{
  An implementation of the Nelder-Mead algorithm for derivative-free
  optimization / function minimization.
}
\usage{
nelder_mead(x0, fn, maxfeval = 5000, scale = 1, tol = 1e-10, ...)

nelmin(x0, fn, maxfeval = 50000, scale = 1, tol = 1e-10, ..., 
            step = rep(1.0, length(x0)))
nelminb(x0, fn, lower, upper, maxfeval = 10000, tol = 1e-10, ..., 
            step = rep(1, length(x0)))
}
\arguments{
  \item{x0}{starting vector.}
  \item{fn}{nonlinear function to be minimized.}
  \item{maxfeval}{maximum number of function calls.}
  \item{scale}{scale factor, for -1 the maximum will be searched for.}
  \item{tol}{relative tolerance, to be used as stopping rule.}
  \item{step}{size and shape of initial simplex; relative magnitudes of
              its elements should reflect the units of the variables.}
  \item{lower, upper}{lower and upper of a bounded region.}
  \item{\ldots}{additional arguments to be passed to the function.}
}
\details{
  Also called a `simplex' method for finding the local minimum of a function
  of several variables. The method is a pattern search that compares function
  values at the vertices of the simplex. The process generates a sequence of
  simplices with ever reducing sizes.

  `nelmin()' is a more elaborate version of Nelder-Mead, including restarts. It
  can be used up to 20 dimensions and more (than `tol' and `maxfeval need to be
  increased), while `nelder_mead' should only be used up to 10 dimensions.

  `nelminb()' applies a transformation of bounded to unbounded regions before
  utilizing Nelder-Mead. Of course, if the optimum is near to the boundary,
  results will not be as accurate as in the interior.
  
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
}
\note{
  Original FORTRAN77 version by R O'Neill; MATLAB version by John Burkardt
  under LGPL license. Re-implemented in R by Hans W. Borchers. For another
  elaborate implementation of Nelder-Mead see the package `dfoptim'.

  `nelder_mead()' used Matlab code as provided in the textbook by Mathews/Fink.
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
nelder_mead(c(0, 0), rosenbrock)
# $xmin
# [1] 0.9999969 0.9999936
# $fmin
# [1] 1.131857e-11
# $nfeval
# [1] 47

nelmin(c(0, 0), rosenbrock)
# $xmin
# [1] 1 1
# $fmin
# [1] 1.389895e-17
# $nfeval
# [1] 200
# $restarts
# [1] 0

nelminb(c(0, 0), rosenbrock, c(-0.5,-0.5), c(0.5,0.5))
# $xmin
# [1] 0.50 0.25
# $fmin
# [1] 0.25
# $nfeval
# [1] 195
# $restarts
# [1] 0
}
\keyword{ optimize }
