\name{givens}
\alias{givens}
\title{Givens Rotation}
\description{
  Givens Rotations and QR decomposition
}
\usage{
givens(A)
}
\arguments{
  \item{A}{numeric square matrix.}
}
\details{
  \code{givens(A)} returns a QR decomposition (or factorization) of the
  square matrix \code{A} by applying unitary 2-by-2 matrices \code{U} such
  that \code{U * [xk;xl] = [x,0]} where \code{x=sqrt(xk^2+xl^2)}
}
\value{
  List with two matrices \code{Q} and \code{R}, \code{Q} orthonormal and
  \code{R} upper triangular, such that \code{A=Q\%*\%R}.
}
\references{
  Golub, G. H., and Ch. F. van Loan (1996). Matrix Computations.
  Third edition, John Hopkins University Press, Baltimore.
}
\seealso{
  \code{\link{householder}}
}
\examples{
##  QR decomposition
A <- matrix(c(0,-4,2, 6,-3,-2, 8,1,-1), 3, 3, byrow=TRUE)
gv <- givens(A)
(Q <- gv$Q); (R <- gv$R)
zapsmall(Q \%*\% R)

givens(magic(5))
}
\keyword{ array }
