\name{subspace}
\alias{subspace}
\title{
  Angle between two subspaces
}
\description{
  Finds the angle between two subspaces.
}
\usage{
subspace(A, B)
}
\arguments{
  \item{A, B}{Numeric matrices; vectors will be considered as column vectors.
              These matrices must have the same number or rows.}
}
\details{
  Finds the angle between two subspaces specified by the columns of A and B.
}
\value{
  An angle in radians.
}
\references{
  Strang, G. (1998). Introduction to Linear Algebra. Wellesley-Cambridge Press.
}
\note{
  It is not necessary that two subspaces be the same size in order to find
  the angle between them. Geometrically, this is the angle between two
  hyperplanes embedded in a higher dimensional space.
}
\seealso{
  \code{\link{orth}}
}
\examples{
180 * subspace(c(1, 2), c(2, 1)) / pi  #=> 36.87
180 * subspace(c(0, 1), c(1, 2)) / pi  #=> 26.565

H <- hadamard(8)
A <- H[, 2:4]
B <- H[, 5:8]
subspace(A, B)    #=> 1.5708 or pi/2, i.e. A and B are orthogonal
}
\keyword{ array }
