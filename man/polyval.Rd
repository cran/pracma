\name{polyval, polyvalm}
\alias{polyval}
\alias{polyvalm}
\title{Evaluating a Polynomial}
\description{
  Evaluate polynomial on vector or matrix.
}
\usage{
  polyval(p, x)

  polyvalm(p, A)
}
\arguments{
  \item{p}{vector representing a polynomial.}
  \item{x}{vector of values where to evaluate the polynomial.}
  \item{A}{matrix; needs to be square.}
}
\details{
  \code{polyval} valuates the polynomial given by \code{p} at the 
  values specified by the elements of \code{x}. If \code{x} is 
  a matrix, the polynomial will be evaluated at each element and  
  a matrix returned.

  \code{polyvalm} will evaluate the polynomial in the matrix sense,
  i.e., matrix multiplication is used instead of element by element
  multiplication as used in 'polyval'. The argument matrix \code{A}
  must be a square matrix.
}
\value{
  Vector of values, resp. a matrix.
}
\seealso{
  \code{\link{poly}}, \code{\link{roots}}
}
\examples{
  # Evaluate 3 x^2 + 2 x + 1 at x = 5, 7, and 9
  p = c(3, 2, 1);
  polyval(p, c(5, 7, 9))    # 86  162  262

  # Apply the characteristic polynomial to its matrix
  A <- pascal(4)
  p <- pracma::Poly(A)      # characteristic polynomial of A
  polyvalm(p, A)            # almost zero 4x4-matrix
}
\keyword{ math }
