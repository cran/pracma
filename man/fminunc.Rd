\name{fminunc}
\alias{fminunc}
\title{
  Minimize Unconstrained Multivariable Function
}
\description{
  Find minimum of unconstrained multivariable functions.
}
\usage{
  fminunc(x0, fn, gr = NULL, ...,
          tol = 1e-08, maxiter = 0, maxfeval = 0)
}
\arguments{
  \item{x0}{starting point.}
  \item{fn}{objective function to be minimized.}
  \item{gr}{gradient function of the objective.}
  \item{...}{additional parameters to be passed to the function.}
  \item{tol}{relative tolerance.}
  \item{maxiter}{maximum number of iterations.}
  \item{maxfeval}{maximum number of function evaluations.}
}
\details{
  The method used here for unconstrained minimization is a variant of a
  "variable metric" resp. quasi-Newton approach.
}
\value{
  List with the following components:
  \item{par}{the best minimum found.}
  \item{value}{function value at the minimum.}
  \item{counts}{number of function and gradient calls.}
  \item{convergence}{integer indicating the terminating situation.}
  \item{message}{description of the final situation.}
}
\references{
  J. Nocedal and S. J. Wright (2006). Numerical Optimization. Second
  Edition, Springer Science+Business Media, New York.
}
\note{
  \code{fminunc} mimics the Matlab function of the same name.
}
\author{
  The "variable metric" code provided by John Nash (package Rvmmin),
  stripped-down version by Hans W. Borchers.
}
\seealso{
  \code{\link{fminsearch}}, \code{\link{fmincon}}, 
}
\examples{
  fun = function(x) 
          x[1]*exp(-(x[1]^2 + x[2]^2)) + (x[1]^2 + x[2]^2)/20
  fminunc(x0 = c(1, 2), fun)
  ## xmin: c(-0.6691, 0.0000); fmin: -0.4052
}
\keyword{ optimize }
