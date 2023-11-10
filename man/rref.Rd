\name{rref}
\alias{rref}
\title{
  Reduced Row Echelon Form
}
\description{
  Produces the reduced row echelon form of \code{A} using
  Gauss Jordan elimination with partial pivoting.
}
\usage{
rref(A)
}
\arguments{
  \item{A}{numeric matrix.}
}
\details{
  A matrix of ``row-reduced echelon form" has the following characteristics:

  1. All zero rows are at the bottom of the matrix

  2. The leading entry of each nonzero row after the first occurs
     to the right of the leading entry of the previous row.

  3. The leading entry in any nonzero row is 1.

  4. All entries in the column above and below a leading 1 are zero.

  Roundoff errors may cause this algorithm to compute a different value
  for the rank than \code{rank}, \code{orth} or \code{null}.
}
\value{
  A matrix the same size as \code{m}.
}
\note{
  This serves demonstration purposes only; don't use for large matrices.
}
\references{
  Weisstein, Eric W. ``Echelon Form." From MathWorld -- A Wolfram Web Resource.\cr
  \url{https://mathworld.wolfram.com/EchelonForm.html}
}
\seealso{
  \code{\link{qr.solve}}
}
\examples{
A <- matrix(c(1, 2, 3, 1, 3, 2, 3, 2, 1), 3, 3, byrow = TRUE)
rref(A)       
#      [,1] [,2] [,3]
# [1,]    1    0    0
# [2,]    0    1    0
# [3,]    0    0    1

A <- matrix(data=c(1, 2, 3, 2, 5, 9, 5, 7, 8,20, 100, 200),
            nrow=3, ncol=4, byrow=FALSE)  
rref(A)
#   1    0    0  120
#   0    1    0    0
#   0    0    1  -20

# Use rref on a rank-deficient magic square:
A = magic(4)
R = rref(A)
zapsmall(R)
#   1    0    0    1
#   0    1    0    3
#   0    0    1   -3
#   0    0    0    0
}
\keyword{ math }
