\name{eig}
\alias{eig}
\title{Eigenvalue Function (Matlab Style)}
\description{
  Eigenvalues of a matrix
}
\usage{
  eig(a)
}
\arguments{
  \item{a}{real or complex square matrix}
}
\details{
  Computes the eigenvalues of a square matrix of real or complex numbers,
  using the R routine \code{eigen} without computing the eigenvectors.
}
\value{
  Vector of eigenvalues
}
\seealso{
  \code{\link{compan}}
}
\examples{
  eig(matrix(c(1,-1,-1,1), 2, 2))   #=> 2 0
  eig(matrix(c(1,1,-1,1), 2, 2))    # complex values
  eig(matrix(c(0,1i,-1i,0), 2, 2))  # real values
}
\keyword{ array }
