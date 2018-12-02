\name{fmincon}
\alias{fmincon}
\title{
  Minimize Nonlinear Constrained Multivariable Function.
}
\description{
  Find minimum of multivariable functions with nonlinear constraints.
}
\usage{
  fmincon(x0, fn, gr = NULL, ..., method = "SQP",
          A = NULL, b = NULL, Aeq = NULL, beq = NULL,
          lb = NULL, ub = NULL, hin = NULL, heq = NULL,
          tol = 1e-06, maxfeval = 10000, maxiter = 5000)
}
\arguments{
  \item{x0}{starting point.}
  \item{fn}{objective function to be minimized.}
  \item{gr}{gradient function of the objective; not used for SQP method.}
  \item{...}{additional parameters to be passed to the function.}
  \item{method}{method options 'SQP', 'auglag'; only 'SQP is implemented.}
  \item{A, b}{linear ineqality constraints of the form A x <= b .}
  \item{Aeq, beq}{linear eqality constraints of the form Aeq x = beq .}
  \item{lb, ub}{bounds constraints of the form lb <= x <= ub .}
  \item{hin}{nonlinear inequality constraints of the form hin(x) <= 0 .}
  \item{heq}{nonlinear equality constraints of the form heq(x) = 0 .}
  \item{tol}{relative tolerance.}
  \item{maxiter}{maximum number of iterations.}
  \item{maxfeval}{maximum number of function evaluations.}
}
\details{
  Wraps the function \code{solnl} in the 'NlcOptim' package. The
  underlying method is a Squential Quadratic Programming (SQP) approach.

  Constraints can be defined in different ways, as linear constraints in
  matrix form, as nonlinear functions, or as bounds constraints.
}
\value{
  List with the following components:
  \item{par}{the best minimum found.}
  \item{value}{function value at the minimum.}
  \item{convergence}{integer indicating the terminating situation.}
  \item{info}{parameter list describing the final situation.}
}
\references{
  J. Nocedal and S. J. Wright (2006). Numerical Optimization. Second
  Edition, Springer Science+Business Media, New York.
}
\note{
  \code{fmincon} mimics the Matlab function of the same name.
}
\author{
  Xianyan Chen for the package NlcOptim.
}
\seealso{
  \code{\link{fminsearch}}, \code{\link{fminunc}}, 
}
\examples{
# Classical Rosenbrock function
n <- 10; x0 <- rep(1/n, n)
fn <- function(x) {n <- length(x)
    x1 <- x[2:n]; x2 <- x[1:(n - 1)]
    sum(100 * (x1 - x2^2)^2 + (1 - x2)^2)
}
# Equality and inequality constraints
heq1 <- function(x) sum(x)-1.0
hin1 <- function(x) -1 * x
hin2 <- function(x) x - 0.5
ub <- rep(0.5, n)

# Apply constraint minimization
res <- fmincon(x0, fn, hin = hin1, heq = heq1)
res$par; res$value
}
\keyword{ optimize }
