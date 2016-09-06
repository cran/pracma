\name{nearest_spd}
\alias{nearest_spd}
\title{
  Nearest Symmetric Positive-definite Matrix
}
\description{
  Find nearest (in Frobenius norm) symmetric positive-definite matrix to A.
}
\usage{
nearest_spd(A)
}
\arguments{
  \item{A}{square numeric matrix.}
}
\details{
  "The nearest symmetric positive semidefinite matrix in the
  Frobenius norm to an arbitrary real matrix A is shown to be (B + H)/2,
  where H is the symmetric polar factor of B=(A + A')/2."\cr
  N. J. Highham
}
\value{
  Returns a matrix of the same size.
}
\references{
  Nicholas J. Higham (1988). Computing a nearest symmetric positive 
  semidefinite matrix. 
  Linear Algebra and its Applications. Vol. 103, pp.103-118.
}
\seealso{
  \code{\link{randortho}}, \code{\link{procrustes}}
}
\examples{
A <- matrix(1:9, 3, 3)
B <- nearest_spd(A); B
#          [,1]     [,2]     [,3]
# [1,] 2.034900 3.202344 4.369788
# [2,] 3.202344 5.039562 6.876781
# [3,] 4.369788 6.876781 9.383774
norm(B - A, type = 'F')
# [1] 3.758517
}
\keyword{ array }
