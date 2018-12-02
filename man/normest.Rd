\name{normest}
\alias{normest}
\title{
  Estimated Matrix Norm
}
\description{
  Estimate the 2-norm of a real (or complex-valued) matrix.
  2-norm is also the maximum absolute eigenvalue of M, computed here
  using the power method.
}
\usage{
normest(M, maxiter = 100, tol = .Machine$double.eps^(1/2))
}
\arguments{
  \item{M}{Numeric matrix; vectors will be considered as column vectors.}
  \item{maxiter}{Maximum number of iterations allowed; default: 100.}
  \item{tol}{Tolerance used for stopping the iteration.}
}
\details{
  Estimate the 2-norm of the matrix \code{M}, typically used for large or
  sparse matrices, where the cost of calculating the \code{norm (A)} is
  prohibitive and an approximation to the 2-norm is acceptable.
  
  Theoretically, the 2-norm of a matrix \eqn{M} is defined as

  \eqn{||M||_2 = max \frac{||M*x||_2}{||x||_2}} for all \eqn{x \neq 0}

  where \eqn{||.||_2} is the Euclidean/Frobenius norm.
}
\value{
  2-norm of the matrix as a positive real number.
}
\references{
  Trefethen, L. N., and D. Bau III. (1997). Numerical Linear Algebra. SIAM,
  Philadelphia.   
}
\note{
  If feasible, an accurate value of the 2-norm would simply be calculated
  as the maximum of the singular values (which are all positive):

  \code{max(svd(M)\$d)}
}
\seealso{
  \code{\link{cond}}, \code{\link{svd}}
}
\examples{
normest(magic(5)) == max(svd(magic(5))$d)  # TRUE
normest(magic(100))                        # 500050
}
\keyword{ array }
