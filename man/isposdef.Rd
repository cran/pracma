\name{isposdef}
\alias{isposdef}
\title{
  Positive Definiteness
}
\description{
  Test for positive definiteness.
}
\usage{
isposdef(A, psd = FALSE, tol = 1e-10)
}
\arguments{
  \item{A}{symmetric matrix}
  \item{psd}{logical, shall semi-positive definiteness be tested?}
  \item{tol}{tolerance to check symmetry and Cholesky decomposition.}
}
\details{
  Whether matrix \code{A} is positive definite will be determined by
  applying the Cholesky decomposition. The matrix must be symmetric.

  With \code{psd=TRUE} the matrix will be tested for being semi-positive
  definite. If not positive definite, still a warning will be generated.
}
\value{
  Returns \code{TRUE} or \code{FALSE}.
}
\examples{
A <- magic(5)
# isposdef(A)
## [1] FALSE
## Warning message:
## In isposdef(A) : Matrix 'A' is not symmetric.
## FALSE

A <- t(A) \%*\% A
isposdef(A)
## [1] TRUE

A[5, 5] <- 0
isposdef(A)
## [1] FALSE
}
\keyword{ array }
