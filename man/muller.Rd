\name{muller}
\alias{muller}
\title{
  Muller's Method
}
\description{
  Muller's root finding method, similar to the secant method, using a 
  parabola through three points for approximating the curve.
}
\usage{
muller(f, p0, p1, p2 = NULL, maxiter = 100, tol = 1e-10)
}
\arguments{
  \item{f}{function whose root is to be found; function needs to be defined
           on the complex plain.}
  \item{p0, p1, p2}{three starting points, should enclose the assumed root.}
  \item{tol}{relative tolerance, change in successive iterates.}
  \item{maxiter}{maximum number of iterations.}
}
\details{
  Generalizes the secant method by using parabolic interpolation between
  three points. This technique can be used for any root-finding problem, 
  but is particularly useful for approximating the roots of polynomials,
  and for finding zeros of analytic functions in the complex plane.
}
\value{
  List of \code{root}, \code{fval}, \code{niter}, and \code{reltol}.
}
\note{
  Muller's method is considered to be (a bit) more robust than Newton's.
}
\references{
  Pseudo- and C code available from the `Numerical Recipes';
  pseudocode in the book `Numerical Analysis' by Burden and Faires (2011).
}
\seealso{
  \code{\link{secant}}, \code{\link{newtonRaphson}}, \code{\link{newtonsys}}
}
\examples{
muller(function(x) x^10 - 0.5, 0, 1)  # root: 0.9330329915368074

f <- function(x) x^4 - 3*x^3 + x^2 + x + 1
p0 <- 0.5; p1 <- -0.5; p2 <- 0.0
muller(f, p0, p1, p2)
## $root
## [1] -0.3390928-0.4466301i
## ...

##  Roots of complex functions:
fz <- function(z) sin(z)^2 + sqrt(z) - log(z)
muller(fz, 1, 1i, 1+1i)
## $root
## [1] 0.2555197+0.8948303i
## $fval
## [1] -4.440892e-16+0i
## $niter
## [1] 8
## $reltol
## [1] 3.656219e-13
}
\keyword{ math }
