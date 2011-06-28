\name{muller}
\alias{muller}
\title{
  Muller's Method
}
\description{
  Muller's root finding method.
}
\usage{
muller(f, x1, x2, tol = .Machine$double.eps^0.5, kmax = 24)
}
\arguments{
  \item{f}{function whose root is to be found.}
  \item{x1, x2}{two starting estimates, should bracket the assumed root.}
  \item{tol}{relative tolerance, change in successive iterates.}
  \item{kmax}{maximum number of iterations.}
}
\details{
  Generalizes the secant method by using quadratic interpolation between
  three points.

  Can be used to find zeros of analytic functions in the complex plane. For
  this purpose, a complex sign function has been inside \code{muller}.
  But think of it, convergence is much slower in this case and \code{kmax}
  should at least be doubled.
}
\value{
  List of \code{root} and \code{fval}.
}
\note{
  Muller's method is considered to be (a bit) more robust than Newton's.
}
\author{
  Pseudo- and C code available from `Numeric Recipes', Matlab code in the
  book by Fausett, R code adopted by Hans W Borchers.
}
\references{
  Fausett, L. V. (2008). Applied Numerical Analysis Using Matlab.
  Second Edition, Pearson Education.
}
\seealso{
  \code{\link{secant}}, \code{\link{newtonRaphson}}, \code{\link{newtonsys}}
}
\examples{
muller(function(x) x^10 - 0.5, 0, 1)  # root: 0.9330329915368074

##  Roots of complex functions:
fz <- function(z) sin(z)^2 + sqrt(z) - log(z)
muller(fz, 1, 1i)
}
\keyword{ math }
