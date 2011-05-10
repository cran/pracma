\name{nullspace}
\alias{nullspace}
\title{
  Kernel or Nullspace
}
\description{
  Kernel of the linear map defined by matrix \code{M}.
}
\usage{
nullspace(M)
}
\arguments{
  \item{M}{Numeric matrix; vectors will be considered as column vectors.}
}
\details{
  The kernel (aka null space/nullspace) of a matrix \code{M} is the set of
  all vectors \code{x} for which \code{Ax=0}. It is computed from the
  QR-decomposition of the matrix.
}
\value{
  If \code{M} is an \code{n}-by-\code{m} (operating from right on
  \code{n}-dimensional row vectors), then \code{N=nullspace(M)} is a
  \code{k}-by-\code{n} matrix whose rows define a (linearly independent)
  basis of the \code{k}-dimensional kernel in \code{R^n}.

  As the rank of a matrix is also the dimension of its image, the following
  relation is true:

  \code{n = dim(nullspace(M)) + rank(M)}
}
\references{
  Trefethen, L. N., and D. Bau III. (1997). Numerical Linear Algebra. SIAM,
  Philadelphia. 
}
\note{
  The image of \code{M} can be retrieved from \code{orth()}.
}
\seealso{
  \code{\link{mrank}}, \code{\link{orth}}, \code{MASS::Null}
}
\examples{
M <- matrix(1:12, 3, 4)
mrank(M)                 #=> 2
N <- nullspace(M)
#           [,1]       [,2]      [,3]
# [1,] 0.4082483 -0.8164966 0.4082483
N %*% M                  # almost c(0, 0, 0)

M <- magic(5)
rank(M)                  #=> 5
nullspace(M)             #=> 0 0 0 0 0
}
\keyword{ array }
