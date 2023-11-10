\name{nelder_mead}
\alias{nelder_mead}
\title{
  Nelder-Mead Function Minimization Method
}
\description{
  An implementation of the Nelder-Mead algorithm for derivative-free
  optimization / function minimization.
}
\usage{
nelder_mead(fn, x0, ..., adapt = TRUE,
            tol = 1e-08, maxfeval = 5000, 
			step = rep(1.0, length(x0)))
}
\arguments{
  \item{fn}{nonlinear function to be minimized.}
  \item{x0}{starting point for the iteration.}
  \item{...}{additional arguments to be passed to the function.}
  \item{adapt}{logical; adapt to parameter dimension.}
  \item{tol}{terminating limit for the variance of function values;
             can be made *very* small, like \code{tol=1e-50}.}
  \item{maxfeval}{maximum number of function evaluations.}
  \item{step}{size and shape of initial simplex; relative magnitudes of
              its elements should reflect the units of the variables.}
}
\details{
  Also called a `simplex' method for finding the local minimum of a function
  of several variables. The method is a pattern search that compares function
  values at the vertices of the simplex. The process generates a sequence of
  simplices with ever reducing sizes.

  The simplex function minimisation procedure due to Nelder and Mead (1965),
  as implemented by O'Neill (1971), with subsequent comments by Chambers and 
  Ertel 1974, Benyon 1976, and Hill 1978. For another elaborate implementation 
  of Nelder-Mead in R based on Matlab code by Kelley see package `dfoptim'.

  \code{nelder_mead} can be used up to 20 dimensions (then `tol' and `maxfeval'
  need to be increased). With \code{adapt=TRUE} it applies adaptive
  coefficients for the simplicial search, depending on the problem dimension
  -- see Fuchang and Lixing (2012). This approach especially reduces the
  number of function calls.
}
\value{
  List with following components:
    \item{xmin}{minimum solution found.}
    \item{fmin}{value of \code{f} at minimum.}
    \item{count}{number of iterations performed.}
    \item{info}{list with solver name and no. of restarts.}
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
  under LGPL license. Re-implemented in R by Hans W. Borchers.
}
\seealso{
  \code{\link{hooke_jeeves}}
}
\examples{
##  Classical tests as in the article by Nelder and Mead
# Rosenbrock's parabolic valley
rpv <- function(x) 100*(x[2] - x[1]^2)^2 + (1 - x[1])^2
x0 <- c(-2, 1)
nelder_mead(rpv, x0)                     #  1 1

# Fletcher and Powell's helic valley
fphv <- function(x)
    100*(x[3] - 10*atan2(x[2], x[1])/(2*pi))^2 + 
        (sqrt(x[1]^2 + x[2]^2) - 1)^2 +x[3]^2
x0 <- c(-1, 0, 0)
nelder_mead(fphv, x0)                    #  1 0 0

# Powell's Singular Function (PSF)
psf <- function(x)  (x[1] + 10*x[2])^2 + 5*(x[3] - x[4])^2 + 
                    (x[2] - 2*x[3])^4 + 10*(x[1] - x[4])^4
x0 <- c(3, -1, 0, 1)
# needs maximum number of function calls
nelder_mead(psf, x0, maxfeval=30000)         #  0 0 0 0

\dontrun{
# Can run Rosenbrock's function in 30 dimensions in one and a half minutes:
nelder_mead(fnRosenbrock, rep(0, 30), tol=1e-20, maxfeval=10^7)
# $xmin
#  [1]  0.9999998 1.0000004 1.0000000 1.0000001 1.0000000 1.0000001
#  [7]  1.0000002 1.0000001 0.9999997 0.9999999 0.9999997 1.0000000
# [13]  0.9999999 0.9999994 0.9999998 0.9999999 0.9999999 0.9999999
# [19]  0.9999999 1.0000001 0.9999998 1.0000000 1.0000003 0.9999999
# [25]  1.0000000 0.9999996 0.9999995 0.9999990 0.9999973 0.9999947
# $fmin
# [1] 5.617352e-10
# $fcount
# [1] 1426085
# elapsed time is 96.008000 seconds }
}
\keyword{ optimize }
