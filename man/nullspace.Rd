\name{nullspace}
\alias{nullspace}
\alias{null}
\title{
  Kernel or Nullspace
}
\description{
  Kernel of the linear map defined by matrix \code{M}.
}
\usage{
nullspace(M)
null(M)
}
\arguments{
  \item{M}{Numeric matrix; vectors will be considered as column vectors.}
}
\details{
  The kernel (aka null space/nullspace) of a matrix \code{M} is the set of
  all vectors \code{x} for which \code{Ax=0}. It is computed from the
  QR-decomposition of the matrix.

  \code{null} is simply an alias for \code{nullspace} -- and the Matlab name.
}
\value{
  If \code{M} is an \code{n}-by-\code{m} (operating from left on
  \code{m}-dimensional column vectors), then \code{N=nullspace(M)} is a
  \code{m}-by-\code{k} matrix whose columns define a (linearly independent)
  basis of the \code{k}-dimensional kernel in \code{R^m}.

  If the kernel is only the null vector \code{(0 0 ... 0)}, then NULL will
  be returned.

  As the rank of a matrix is also the dimension of its image, the following
  relation is true:

  \code{m = dim(nullspace(M)) + rank(M)}
}
\references{
  Trefethen, L. N., and D. Bau III. (1997). Numerical Linear Algebra. SIAM,
  Philadelphia. 
}
\note{
  The image of \code{M} can be retrieved from \code{orth()}.
}
\seealso{
  \code{\link{Rank}}, \code{\link{orth}}, \code{MASS::Null}
}
\examples{
M <- matrix(1:12, 3, 4)
Rank(M)                 #=> 2
N <- nullspace(M)
#           [,1]       [,2]      [,3]
# [1,] 0.4082483 -0.8164966 0.4082483
M %*% N                  # almost c(0, 0, 0)

M1 <- matrix(1:6, 2, 3)  # of rank 2
M2 <- t(M1)
nullspace(M1)            # corresponds to 1 -2  1
nullspace(M2)            # NULL, i.e. 0 0

M <- magic(5)
Rank(M)                 #=> 5
nullspace(M)             #=> NULL, i.e. 0 0 0 0 0
}
\keyword{ array }
