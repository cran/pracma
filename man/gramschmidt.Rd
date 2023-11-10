\name{gramSchmidt}
\alias{gramSchmidt}
\title{Gram-Schmidt}
\description{
  Modified Gram-Schmidt Process
}
\usage{
  gramSchmidt(A, tol = .Machine$double.eps^0.5)
}
\arguments{
  \item{A}{numeric matrix with \code{nrow(A)>=ncol(A)}.}
  \item{tol}{numerical tolerance for being equal to zero.}
}
\details{
  The modified Gram-Schmidt process uses the classical orthogonalization
  process to generate step by step an orthonoral basis of a vector space.
  The modified Gram-Schmidt iteration uses orthogonal projectors in order
  ro make the process numerically more stable.
}
\value{
  List with two matrices \code{Q} and \code{R}, \code{Q} orthonormal and
  \code{R} upper triangular, such that \code{A=Q\%*\%R}.
}
\references{
  Trefethen, L. N., and D. Bau III. (1997). Numerical Linear Algebra. SIAM,
  Society for Industrial and Applied Mathematics, Philadelphia.
}
\seealso{
  \code{\link{householder}, \link{givens}}
}
\examples{
##  QR decomposition
A <- matrix(c(0,-4,2, 6,-3,-2, 8,1,-1), 3, 3, byrow=TRUE)
gs <- gramSchmidt(A)
(Q <- gs$Q); (R <- gs$R)
Q \%*\% R  # = A
}
\keyword{ array }
