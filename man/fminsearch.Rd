\name{fminsearch}
\alias{fminsearch}
\title{
  Minimum Finding
}
\description{
  Find minimum of unconstrained multivariable function.
}
\usage{
fminsearch(f, x0, ..., minimize = TRUE, dfree = TRUE,
           maxiter = 1000, tol = .Machine$double.eps^(2/3))
}
\arguments{
  \item{f}{function whose minimum or maximum is to be found.}
  \item{x0}{point considered near to the optimum.}
  \item{minimize}{logical; shall a minimum or a maximum be found.}
  \item{dfree}{logical; apply derivative-free optimization or not.}
  \item{maxiter}{maximal number of iterations}
  \item{tol}{relative tolerance.}
  \item{...}{additional variables to be passed to the function.}
}
\details{
  \code{fminsearch} finds the minimum of a nonlinear scalar multivariable
  function, starting at an initial estimate and returning a value x that is
  a local minimizer of the function.

  With \code{minimize=FALSE} it seaches for a maximum. \code{dfree=TRUE}
  applies Nelder.Mead, else Fletcher-Powell, calculating the derivatives
  numerically.
  
  This is generally referred to as unconstrained nonlinear optimization.
  \code{fminsearch} may only give local solutions.
}
\value{
  List with
  \item{xopt}{location of the location of minimum resp. maximum.}
  \item{fval}{function value at the optimum.}
  \item{niter}{number of iterations.}
}
\references{
  Nocedal, J., and S. Wright (2006). Numerical Optimization.
  Second Edition, Springer-Verlag, New York.
}
\note{
  \code{fminbnd} mimics the Matlab function of the same name.
}
\seealso{
  \code{\link{optim}}
}
\examples{
# Rosenbrock function
rosena <- function(x, a) 100*(x[2]-x[1]^2)^2 + (a-x[1])^2  # min: (a, a^2)

fminsearch(rosena, c(-1.2, 1), a = sqrt(2))
# x = (1.414214 2.000010) , fval = 1.239435e-11

fminsearch(rosena, c(-1.2, 1), dfree=FALSE, a = sqrt(2))
# x = (1.414214 2.000000) , fval = 3.844519e-26
}
\keyword{ optimize }
