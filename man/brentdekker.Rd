\name{brentDekker}
\alias{brentDekker}
\alias{brent}
\title{
  Brent-Dekker Root Finding Algorithm
}
\description{
  Find root of continuous function of one variable.
}
\usage{
brentDekker(fun, a, b, maxiter = 500, tol = 1e-12, ...)
brent(fun, a, b, maxiter = 500, tol = 1e-12, ...)
}
\arguments{
  \item{fun}{function whose root is to be found.}
  \item{a, b}{left and right end points of an interval;
              function values need to be of different sign at the endpoints.}
  \item{maxiter}{maximum number of iterations.}
  \item{tol}{relative tolerance.}
  \item{...}{additional arguments to be passed to the function.}
}
\details{
  \code{brentDekker} implements a version of the Brent-Dekker algorithm,
  a well known root finding algorithms for real, univariate, continuous
  functions. The Brent-Dekker approach is a clever combination of secant
  and bisection with quadratic interpolation.

  \code{brent} is simply an alias for \code{brentDekker}.
}
\value{
  \code{brent} returns a list with
    \item{root}{location of the root.}
    \item{f.root}{funtion value at the root.}
    \item{f.calls}{number of function calls.}
    \item{estim.prec}{estimated relative precision.}
}
\references{
  Quarteroni, A., R. Sacco, and F. Saleri (2007). Numerical Mathematics.
  Second Edition, Springer-Verlag, Berlin Heidelberg.
}
\seealso{
  \code{\link{ridders}}, \code{\link{newtonRaphson}}
}
\examples{
# Legendre polynomial of degree 5
lp5 <- c(63, 0, -70, 0, 15, 0)/8
f <- function(x) polyval(lp5, x)
brent(f, 0.6, 1)                # 0.9061798459 correct to 12 places
}
\keyword{ math }
