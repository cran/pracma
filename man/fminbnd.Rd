\name{fminbnd}
\alias{fminbnd}
\title{
  Finding Function Minimum
}
\description{
  Find minimum of single-variable function on fixed interval.
}
\usage{
fminbnd(f, a, b, ..., maxiter = 1000, maximum = FALSE,
        tol = .Machine$double.eps^(2/3))
}
\arguments{
  \item{f}{function whose minimum or maximum is to be found.}
  \item{a, b}{endpoints of the interval to be searched.}
  \item{maxiter}{maximal number of iterations.}
  \item{maximum}{logical; shall maximum or minimum be found; default FALSE.}
  \item{tol}{relative tolerance.}
  \item{...}{additional variables to be passed to the function.}
}
\details{
  fminbnd finds the minimum of a function of one variable within a fixed
  interval. It applies Brent's algorithm, based on golden section search and
  parabolic interpolation.

  \code{fminbnd} may only give local solutions.
  \code{fminbnd} never evaluates \code{f} at the endpoints.
}
\value{
  List with
  \item{xmin}{location of the minimum resp. maximum.}
  \item{fmin}{function value at the optimum.}
  \item{niter}{number of iterations used.}
  \item{estim.prec}{estimated precision.}
}
\references{
  R. P. Brent (1973). Algorithms for Minimization Without Derivatives.
  Dover Publications, reprinted 2002.
}
\note{
  \code{fminbnd} mimics the Matlab function of the same name and uses the
  Fortran code underlying the R function \code{optimize}.

}
\seealso{
  \code{\link{fibsearch}}, \code{\link{golden_ratio}}
}
\examples{
fminbnd(cos, 3, 4)          # x = 3.141593  , fval = -1
f <- function(x) x^3-2*x-5
fminbnd(f, 0, 2)            # x = 0.8164966 , fval = -6.088662
}
\keyword{ optimize }
