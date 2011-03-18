\name{pinv}
\alias{pinv}
\title{
Pseudoinverse or Generalized Inverse
}
\description{
Compute the pseudoinverse of a matrix.
}
\usage{
pinv(A)
}
\arguments{
  \item{A}{matrix}
}
\details{
  Compute the pseudoinverse \code{B} of a matrix \code{A} using the
  singular value decomposition \code{svd()}.

  The pseudoinverse \eqn{B} solves the problem to minimize
  \eqn{|A x - b|} by setting \eqn{x = B b}

  \code{s <- svd(A)
  D <- diag(s$d); Dinv <- diag(1/s$d)
  U <- s$u; V <- s$v
  U <- s$u; V <- s$v
  X = V Dinv U\'}

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
  The pseudoinverse or `generalized inverse' (aka Moore-Penrose inverse)
  is also provided by the function \code{ginv()} in package `MASS'. It is
  included here in a somewhat simplified way to be independent of that package.
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
