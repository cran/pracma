\name{mldivide}
\alias{mldivide}
\alias{mrdivide}
\title{Matlab backslash operator}
\description{
  Emulate the Matlab backslash operator ``\\'' through QR decomposition.
}
\usage{
mldivide(A, B, pinv = TRUE)
mrdivide(A, B, pinv = TRUE)
}
\arguments{
  \item{A, B}{
  Numerical or complex matrices; \code{A} and \code{B} must have the same 
  number of rows (for \code{mldivide}) or the same number of columns
  (for \code{mrdivide})
  }
  \item{pinv}{logical; shall SVD decomposition be used; default true.}
}
\details{
  \code{mldivide} performs matrix left division (and \code{mrdivide} matrix
  right division). If \code{A} is scalar it performs element-wise division.

  If \code{A} is square, \code{mldivide} is roughly the same as
  \code{inv(A) \%*\% B} except it is computed in a different way ---
  using QR decomposition.

  If \code{pinv = TRUE}, the default, the SVD will be used as
  \code{pinv(t(A)\%*\%A)\%*\%t(A)\%*\%B} to generate results similar
  to Matlab. Otherwise, \code{qr.solve} will be used.

  If \code{A} is not square, \code{x <- mldivide(A, b)} returnes a
  least-squares solution that minimizes the length of the vector
  \code{A \%*\% x - b}
  (which is equivalent to \code{norm(A \%*\% x - b, "F")}.
}
\value{
  If \code{A} is an n-by-p matrix and \code{B} n-by-q, then the result of
  \code{mldivide(A, B)} is a p-by-q matrix (\code{mldivide}).
}
\note{
  \code{mldivide(A, B)} corresponds to \code{A\\B} in Matlab notation.
}
\examples{
# Solve a system of linear equations
A <- matrix(c(8,1,6, 3,5,7, 4,9,2), nrow = 3, ncol = 3, byrow = TRUE)
b <- c(1, 1, 1)
mldivide(A, b)  # 0.06666667 0.06666667 0.06666667

A <- rbind(1:3, 4:6)
mldivide(A, c(1,1))                 # -0.5  0  0.5 ,i.e. Matlab/Octave result
mldivide(A, c(1,1), pinv = FALSE)   # -1    1  0         R    qr.solve result
}
\keyword{ math }
