\name{fminsearch}
\alias{fminsearch}
\title{
  Minimum Finding
}
\description{
  Find minimum of unconstrained multivariable function.
}
\usage{
fminsearch(f, x0, ..., minimize = TRUE, tol = .Machine$double.eps^(2/3))
}
\arguments{
  \item{f}{function whose minimum or maximum is to be found.}
  \item{x0}{point considered near to the optimum.}
  \item{minimize}{logical; shall a minimum or a maximum be found.}
  \item{tol}{relative tolerance.}
  \item{...}{additional variables to be passed to the function.}
}
\details{
  \code{fminsearch} finds the minimum of a nonlinear scalar multivariable
  function, starting at an initial estimate and returning a value x that is
  a local minimizer of the function.
  
  This is generally referred to as unconstrained nonlinear optimization.
  \code{fminsearch} may only give local solutions.
}
\value{
  List with
  \item{x}{location of the location of minimum resp. maximum.}
  \item{fval}{function value at the optimum.}
}
\references{
  Nocedal, J., and S. Wright (2006). Numerical Optimization.
  Second Edition, Springer-Verlag, New York.
}
\note{
  \code{fminbnd} mimics the Matlab function of the same name and uses the
  R function \code{optim} with method `Nelder-Mead'.
  Will be replaced with own version of Nelder Mead.
}
\seealso{
  \code{\link{optim}}
}
\examples{
# Rosenbrock function
rosena <- function(x, a) 100*(x[2]-x[1]^2)^2 + (a-x[1])^2  # min: (a, a^2)
fminsearch(rosena, c(-1.2, 1), a = sqrt(2))
# x = (1.414218 2.000013) , fval = 4.115895e-11
}
\keyword{ optimize }
