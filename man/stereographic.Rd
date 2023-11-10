\name{stereographic}
\alias{stereographic}
\alias{stereographic_inv}
\title{
  Stereographic Projection
}
\description{
  The stereographic projection is a function that maps the n-dimensional
  sphere from the South pole (0,...,-1) to the tangent plane of the sphere
  at the north pole (0,...,+1).
}
\usage{
stereographic(p)

stereographic_inv(q)
}
\arguments{
  \item{p}{point on the n-spere ; can also be a set of points,
           each point represented as a column of a matrix.}
  \item{q}{point on the tangent plane at the north pole
           (last coordinate = 1); can also be a set of such points.}
}
\details{
The stereographic projection is a smooth function from \eqn{S^n - (0,\dots,-1)}
to the tangent hyperplane at the north pole. The south pole is mapped to
infinity, that is why one speaks of \eqn{S^n} as a 'one-point compactification'
of \eqn{R^{n-1}}.

All mapped points will have a last coordinate 1.0 (lying on the tangent
plane.) Points mapped by 'stereographic_inv' are assumed to have a last
coordinate 1.0 (this will not be checked), otherwise the result will be
different from what is expected -- though the result is still correct in
itself.

All points are column vectors: \code{stereographic} will transform a row
vector to column; \code{stereographic_inv} will return a single vector
as column.
}
\value{
Returns a point (or a set of point) of (n-1) dimensions on the tangent plane
resp. an n-dimensional point on the n-sphere, i.e., \code{sum(x^2) = 1}.
}
\references{
  See the "Stereographic projection" article on Wikipedia.
}
\author{
  Original MATLAB code by J.Burkardt under LGPL license; rewritten in R
  by Hans W Borchers.
}
\note{
  To map a region around the south pole, a similar function would be possible.
  Instead it is simpler to change the sign of the last coordinate.
}
\examples{
# points in the xy-plane (i.e., z = 0)
A <- matrix(c(1,0,0, -1,0,0, 0,1,0, 0,-1,0), nrow = 3)
B <- stereographic(A); B
##      [,1] [,2] [,3] [,4]
## [1,]    2   -2    0    0
## [2,]    0    0    2   -2
## [3,]    1    1    1    1

stereographic_inv(B)
##      [,1] [,2] [,3] [,4]
## [1,]    1   -1    0    0
## [2,]    0    0    1   -1
## [3,]    0    0    0    0

stereographic_inv(c(2,0,2))     # not correct: z = 2
##      [,1]
## [1,]  1.0
## [2,]  0.0
## [3,]  0.5

\dontrun{
# Can be used for optimization with sum(x^2) == 1
# Imagine to maximize the product x*y*z for x^2 + y^2 + z^2 == 1 !
  fnObj <- function(x) {                # length(x) = 2
    x1 <- stereographic_inv(c(x, 1))    # on S^2
    return( -prod(x1) )                 # Maximize
  }
  sol <- optim(c(1, 1), fnObj)
  -sol$value                            # the maximal product
  ## [1] 0.1924501                      #   1/3 * sqrt(1/3)
  stereographic_inv(c(sol$par, 1))      # the solution coordinates
               [,1]                     #   on S^2
  ## [1,] 0.5773374                     # by symmetry must be
  ## [2,] 0.5773756                     # sqrt(1/3) = 0.5773503...
  ## [3,] 0.5773378}
}
\keyword{ manip }
