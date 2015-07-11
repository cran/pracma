\name{orth}
\alias{orth}
\title{
  Range Space
}
\description{
  Range space or image of a matrix.
}
\usage{
orth(M)
}
\arguments{
  \item{M}{Numeric matrix; vectors will be considered as column vectors.}
}
\details{
   \code{B=orth(A)} returns an orthonormal basis for the range of \code{A}.
   The columns of \code{B} span the same space as the columns of \code{A},
   and the columns of \code{B} are orthogonal to each other.

  The number of columns of \code{B} is the rank of \code{A}.
}
\value{
  Matrix of orthogonal columns, spanning the image of \code{M}.
}
\references{
  Trefethen, L. N., and D. Bau III. (1997). Numerical Linear Algebra. SIAM,
  Philadelphia. 
}
\seealso{
  \code{\link{nullspace}}
}
\examples{
M <- matrix(1:12, 3, 4)
Rank(M)                 #=> 2
orth(M)
}
\keyword{ array }
