\name{pinv}
\alias{pinv}
\title{
  Pseudoinverse or Generalized Inverse
}
\description{
  Computes the Moore-Penrose generalized inverse of a matrix.
}
\usage{
pinv(A, tol=.Machine$double.eps^(2/3))
}
\arguments{
  \item{A}{real or complex matrix}
  \item{tol}{tolerance used for assuming an eigenvalue is zero.}
}
\details{
  Compute the generalized inverse \code{B} of a matrix \code{A} using the
  singular value decomposition \code{svd()}. This generalized invers is
  characterized by this equation: \code{A \%*\% B \%*\% A == A}

  The pseudoinverse \eqn{B} solves the problem to minimize
  \eqn{|A x - b|} by setting \eqn{x = B b}

  \code{s <- svd(A)}\cr
  \code{D <- diag(s\$d)}\cr
  \code{Dinv <- diag(1/s\$d)}\cr
  \code{U <- s\$u; V <- s\$v}\cr
  \code{X = V Dinv t(U)}

  Thus \code{B} is computed as \code{s$v \%*\% diag(1/s$d) \%*\% t(s$u)}.
}
\value{
  The pseudoinverse of matrix \code{A}.
}
\references{
  Ben-Israel, A., and Th. N. E. Greville (2003). Generalized Inverses - 
  Theory and Applications. Springer-Verlag, New York.
}
\note{
  The pseudoinverse or `generalized inverse' is also provided by the function
  \code{ginv()} in package `MASS'. It is included in a somewhat simplified
  way to be independent of that package.
}
\seealso{
\code{MASS::ginv}
}
\examples{
A <- matrix(c(7,6,4,8,10,11,12,9,3,5,1,2), 3, 4)
b <- apply(A, 1, sum)  # 32 16 20  row sum
x <- pinv(A) \%*\% b
A \%*\% x              #=> 32 16 20  as column vector
}
\keyword{ array }
