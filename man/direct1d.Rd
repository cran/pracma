\name{direct1d}
\alias{direct1d}
\title{
  Univariate Global Optimization
}
\description{
  Implementation of the DIRECT global optimization algorithm in the
  one-dimensional case.
}
\usage{
direct1d(f, a, b, maxiter = 20, ...)
}
\arguments{
  \item{f}{function to be minimized.}
  \item{a, b}{end points of the interval, \code{a<b}.}
  \item{maxiter}{maximum number of iterations.}
  \item{...}{further parameters to be passed to the function.}
}
\details{
  The DIRECT algorithm for the one-dimensional case is directly derived from
  Shubert's algorithm. Instead of computing the function at the endpoints of
  the interval, it is computed at the midpoint. Intervals ar split in three
  parts, sparing one function evaluation.
}
\value{
  List with components \code{xmin} and \code{fmin}, the minimum found so far
  and its function value.
}
\note{
  The subroutine for finding the set of optimal subintervals is slow and
  has to be intelligently refitted.
}
\author{
  HwB  email: <hwborchers@googlemail.com>
}
\references{
  Jones, D. R., C. D. Perttunen, and B. E. Stuckman (1993). Lipschitzian
  Optimization Without the Lipschitz Constant. Journal of Optimization
  Theory and Application, Vol. 79. No. 1, pp. 157ff.

  Finkel, D., and C. Kelley (2006). Additive Scaling and the DIRECT Algorithm.
  Journal of Global Optimization, Vol. 36, No. 4, pp. 597--608.
}
\seealso{
  \code{\link{findmins}}, \code{dfoptim::direct}
}
\examples{
f <- function(x) sin(10*pi*x) + 0.5*(x-0.5)^2
a <- 0; b <- 1
direct1d(f, 0, 1, maxiter = 20)
# $xmin:  0.5499493794 (error: 3.5e-6)
# $fmin: -0.9987512652
\dontrun{
ezplot(f, a, b, 1000)}
}
\keyword{ optimize }
