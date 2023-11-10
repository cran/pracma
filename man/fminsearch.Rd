\name{fminsearch}
\alias{fminsearch}
\title{
  Derivative-free Nonlinear Function Minimization
}
\description{
  Find minimum of multivariable functions using derivative-free methods.
}
\usage{
fminsearch(fn, x0, ..., lower = NULL, upper = NULL,
           method = c("Nelder-Mead", "Hooke-Jeeves"),
           minimize = TRUE, maxiter = 1000, tol = 1e-08)
}
\arguments{
  \item{fn}{function whose minimum or maximum is to be found.}
  \item{x0}{point considered near to the optimum.}
  \item{...}{additional variables to be passed to the function.}
  \item{lower, upper}{lower and upper bounds constraints.}
  \item{method}{"Nelder-Mead" (default) or "Hooke-Jeeves"; can be abbreviated.}
  \item{minimize}{logical; shall a minimum or a maximum be found.}
  \item{maxiter}{maximal number of iterations}
  \item{tol}{relative tolerance.}
}
\details{
  \code{fminsearch} finds the minimum of a nonlinear scalar multivariable
  function, starting at an initial estimate and returning a value x that is
  a local minimizer of the function. With \code{minimize=FALSE} it searches
  for a maximum, by default for a (local) minimum.

  As methods/solvers "Nelder-Mead" and "Hooke-Jeeves" are available. Only
  Hooke-Jeeves can handle bounds constraints. For nonlinear constraints see
  \code{fmincon}, and for methods using gradients see \code{fminunc}.

  Important: \code{fminsearch} may only give local solutions.
}
\value{
  List with
  \item{xopt}{location of the location of minimum resp. maximum.}
  \item{fmin}{function value at the optimum.}
  \item{count}{number of function calls.}
  \item{convergence}{info about convergence: not used at the moment.}
  \item{info}{special information from the solver.}
}
\references{
  Nocedal, J., and S. Wright (2006). Numerical Optimization.
  Second Edition, Springer-Verlag, New York.
}
\note{
  \code{fminsearch} mimics the Matlab function of the same name.
}
\seealso{
  \code{\link{nelder_mead}}, \code{\link{hooke_jeeves}}
}
\examples{
# Rosenbrock function
rosena <- function(x, a) 100*(x[2]-x[1]^2)^2 + (a-x[1])^2  # min: (a, a^2)

fminsearch(rosena, c(-1.2, 1), a = sqrt(2), method="Nelder-Mead")
## $xmin                   $fmin
## [1] 1.414292 2.000231   [1] 1.478036e-08

fminsearch(rosena, c(-1.2, 1), a = sqrt(2), method="Hooke-Jeeves")
## $xmin                   $fmin
## [1] 1.414215 2.000004   [1] 1.79078e-12
}
\keyword{ optimize }
