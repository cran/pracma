\name{eigjacobi}
\alias{eigjacobi}
\title{
  Jacobi Eigenvalue Method
}
\description{
  Jacobi's iteration method for eigenvalues and eigenvectors.
}
\usage{
eigjacobi(A, tol = .Machine$double.eps^(2/3))
}
\arguments{
  \item{A}{a real symmetric matrix.}
  \item{tol}{requested tolerance.}
}
\details{
  The Jacobi eigenvalue method repeatedly performs (Givens) transformations
  until the matrix becomes almost diagonal.
}
\value{
  Returns a list with components \code{V}, a matrix containing the
  eigenvectors as columns, and \code{D} a vector of the eigenvalues.
}
\note{
  This R implementation works well up to 50x50-matrices. 
}
\references{
  Mathews, J. H., and K. D. Fink (2004). Numerical Methods Using Matlab.
  Fourth edition, Pearson education, Inc., New Jersey.
}
\seealso{
  \code{\link{eig}}
}
\examples{
A <- matrix(c( 1.06, -0.73,  0.77, -0.67,
              -0.73,  2.64,  1.04,  0.72,
               0.77,  1.04,  3.93, -2.14,
              -0.67,  0.72, -2.14,  2.04), 4, 4, byrow = TRUE)
eigjacobi(A)
# $V
#            [,1]       [,2]       [,3]       [,4]
# [1,] 0.87019414 -0.3151209  0.1975473 -0.3231656
# [2,] 0.11138094  0.8661855  0.1178032 -0.4726938
# [3,] 0.07043799  0.1683401  0.8273261  0.5312548
# [4,] 0.47475776  0.3494040 -0.5124734  0.6244140
# 
# $D
# [1] 0.66335457 3.39813189 5.58753257 0.02098098
}
\keyword{ math }
