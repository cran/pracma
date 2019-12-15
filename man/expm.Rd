\name{expm}
\alias{expm}
\alias{logm}
\title{
  Matrix Exponential
}
\description{
  Computes the exponential of a matrix.
}
\usage{
expm(A, np = 128)

logm(A)
}
\arguments{
  \item{A}{numeric square matrix.}
  \item{np}{number of points to use on the unit circle.}
}
\details{
  For an analytic function \eqn{f} and a matrix \eqn{A} the expression
  \eqn{f(A)} can be computed by the Cauchy integral
  \deqn{f(A)  =  (2 \pi i)^{-1} \int_G (zI-A)^{-1} f(z) dz}
  where \eqn{G}  is a closed contour around the eigenvalues of \eqn{A}.

  Here this is achieved by taking G to be a circle and approximating the
  integral by the trapezoid rule.

  \code{logm} is a fake at the moment as it computes the matrix logarithm
  through taking the logarithm of its eigenvalues; will be replaced by an
  approach using Pade interpolation.

  Another more accurate and more reliable approach for computing these
  functions can be found in the R package `expm'.
}
\value{
  Matrix of the same size as \code{A}.
}
\references{
  Moler, C., and Ch. Van Loan (2003). Nineteen Dubious Ways to Compute
  the Exponential of a Matrix, Twenty-Five Years Later.
  SIAM Review, Vol. 1, No. 1, pp. 1--46.

  N. J. Higham (2008). Matrix Functions: Theory and Computation. SIAM
  Society for Industrial and Applied Mathematics.
}
\author{
  Idea and Matlab code for a cubic root by Nick Trefethen in his 
  ``10 digits 1 page'' project.
}
\note{
  This approach could be used for other analytic functions, but a point to
  consider is which branch to take (e.g., for the \code{logm} function).
}
\seealso{
  \code{expm::expm}
}
\examples{
##  The Ward test cases described in the help for expm::expm agree up to
##  10 digits with the values here and with results from Matlab's expm !
A <- matrix(c(-49, -64, 24, 31), 2, 2)
expm(A)
# -0.7357588 0.5518191
# -1.4715176 1.1036382

A1 <- matrix(c(10,  7,  8,  7,
                7,  5,  6,  5,
                8,  6, 10,  9,
                7,  5,  9, 10), nrow = 4, ncol = 4, byrow = TRUE)
expm(logm(A1))
logm(expm(A1))

##  System of linear differential equations: y' = M y  (y = c(y1, y2, y3))
M <- matrix(c(2,-1,1, 0,3,-1, 2,1,3), 3, 3, byrow=TRUE)
M
C1 <- 0.5; C2 <- 1.0; C3 <- 1.5
t  <- 2.0; Mt <- expm(t * M)
yt <- Mt %*% c(C1, C2, C3)       # [y1,y2,y3](t) = [C1, C2, C3] %*% t(Mt)
}
\keyword{ math }
