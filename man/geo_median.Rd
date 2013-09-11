\name{geo_median}
\alias{geo_median}
\title{
  Geometric Median
}
\description{
  Compute the ``geometric median'' of points in n-dimensional space, that is
  the point with the least sum of (Euclidean) distances to all these points.
}
\usage{
geo_median(P, tol = 1e-07, maxiter = 200)
}
\arguments{
  \item{P}{matrix of points, \code{x_i}-coordinates in the ith column.}
  \item{tol}{relative tolerance.}
  \item{maxiter}{maximum number of iterations.}
}
\details{
  The task is solved applying an iterative process, known as Weiszfeld's 
  algorithm. The solution is unique whenever the points are not collinear.

  If the dimension is 1 (one column), the median will be returned.
}
\value{
  Returns a list with components \code{p} the coordinates of the solution 
  point, \code{d} the sum of distances to all the sample points, \code{reltol} 
  the relative tolerance of the iterative process, and \code{niter} the 
  number of iterations.
}
\references{
  See Wikipedia's entry on ``Geometric median''.
}
\note{
  This is also known as the ``1-median problem'' and can be generalized to the 
  ``k-median problem'' for k cluster centers; 
  see \code{kcca} in the `flexclust' package.
}
\seealso{
  \code{\link{L1linreg}}
}
\examples{
# Generate 100 points on the unit sphere in the 10-dim. space
set.seed(1001)
P <- rands(n=100, N=9)
( sol <- geo_median(P) )
# $p
#  [1] -0.009481361 -0.007643410 -0.001252910  0.006437703 -0.019982885 -0.045337987
#  [7]  0.036249563  0.003232175  0.035040592  0.046713023
# $d
# [1] 99.6638
# $reltol
# [1] 3.069063e-08
# $niter
# [1] 10
}

\keyword{ optimize }
