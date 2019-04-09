\name{fletcher_powell}
\alias{fletcher_powell}
\title{
  Fletcher-Powell Conjugate Gradient Minimization
}
\description{
  Conjugate Gradient (CG) minimization through the Davidon-Fletcher-Powell 
  approach for function minimization.

  The Davidon-Fletcher-Powell (DFP) and the Broyden-Fletcher-Goldfarb-Shanno
  (BFGS) methods are the first quasi-Newton minimization methods developed.
  These methods differ only in some details; in general, the BFGS approach
  is more robust.
}
\usage{
fletcher_powell(x0, f, g = NULL,
                maxiter = 1000, tol = .Machine$double.eps^(2/3))
}
\arguments{
  \item{x0}{start value.}
  \item{f}{function to be minimized.}
  \item{g}{gradient function of \code{f};
           if \code{NULL}, a numerical gradient will be calculated.}
  \item{maxiter}{max. number of iterations.}
  \item{tol}{relative tolerance, to be used as stopping rule.}
}
\details{
  The starting point is Newton's method in the multivariate case, when
  the estimate of the minimum is updated by the following equation
  \deqn{x_{new} = x - H^{-1}(x) grad(g)(x)}
  where \eqn{H} is the Hessian and \eqn{grad} the gradient.

  The basic idea is to generate a sequence of good approximations to the
  inverse Hessian matrix, in such a way that the approximations are again
  positive definite.
}
\value{
  List with following components:
    \item{xmin}{minimum solution found.}
    \item{fmin}{value of \code{f} at minimum.}
    \item{niter}{number of iterations performed.}
}
\references{
  J. F. Bonnans, J. C. Gilbert, C. Lemarechal, and C. A. Sagastizabal.
  Numerical Optimization: Theoretical and Practical Aspects. Second Edition,
  Springer-Verlag, Berlin Heidelberg, 2006.
}
\note{
  Used some Matlab code as described in the book ``Applied Numerical Analysis
  Using Matlab'' by L. V.Fausett.
}
\seealso{
  \code{\link{steep_descent}}
}
\examples{
##  Rosenbrock function
rosenbrock <- function(x) {
    n <- length(x)
    x1 <- x[2:n]
    x2 <- x[1:(n-1)]
    sum(100*(x1-x2^2)^2 + (1-x2)^2)
}
fletcher_powell(c(0, 0), rosenbrock)
# $xmin
# [1] 1 1
# $fmin
# [1] 1.774148e-27
# $niter
# [1] 14
}
\keyword{ optimize }
