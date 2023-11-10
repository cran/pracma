\name{L1linreg}
\alias{L1linreg}
\title{
  L1 Linear Regression
}
\description{
  Solve the linear system \code{A x = b} in an Lp sense, that is minimize the
  term \code{sum |b - A x|^p}. The case \code{p=1} is also called 
  ``least absolute deviation'' (LAD) regression.
}
\usage{
L1linreg(A, b, p = 1, tol = 1e-07, maxiter = 200)
}
\arguments{
  \item{A}{matrix of independent variables.}
  \item{b}{independent variables.}
  \item{p}{the p in L^p norm, \code{p<=1}.}
  \item{tol}{relative tolerance.}
  \item{maxiter}{maximum number of iterations.}
}
\details{
  L1/Lp regression is here solved  applying the ``iteratively reweighted least
  square'' (IRLS) method in which each step involves a weighted least squares
  problem.

  If an intercept term is required, add a unit column to \code{A}.
}
\value{
  Returns a list with components \code{x} the linear coefficients describing
  the solution, \code{reltol} the relative tolerance reached, and \code{niter}
  the number of iterations.
}
\references{
  Dasgupta, M., and S.K. Mishra (2004). Least absolute deviation estimation 
  of linear econometric models: A literature review. MPRA Paper No. 1781.
}
\note{
  In this case of \code{p=1}, the problem would be better approached by use
  of linear programming methods.
}
\seealso{
  \code{\link{lm}}, \code{\link{lsqnonlin}}, \code{quantreg::rq}
}
\examples{
m <- 101; n <- 10       # no. of data points, degree of polynomial
x <- seq(-1, 1, len=m)
y <- runge(x)           # Runge's function
A <- outer(x, n:0, '^') # Vandermonde matrix
b <- y

( sol <- L1linreg(A, b) )
# $x
# [1] -21.93242   0.00000  62.91092   0.00000 -67.84854   0.00000
# [7]  34.14400   0.00000  -8.11899   0.00000   0.84533
# 
# $reltol
# [1] 6.712355e-10
# 
# $niter
# [1] 81

# minimum value of polynomial L1 regression
sum(abs(polyval(sol$x, x) - y))
# [1] 3.061811
}

\keyword{ optimize }
