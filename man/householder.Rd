\name{householder}
\alias{householder}
\title{Householder Reflections}
\description{
  Householder reflections and QR decomposition
}
\usage{
  householder(A)
}
\arguments{
  \item{A}{numeric matrix with \code{nrow(A)>=ncol(A)}.}
}
\details{
  The Householder method applies a succession of elementary unitary
  matrices to the left of matrix \code{A}. These matrices are the so-called
  Householder reflections.
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
  \code{\link{givens}}
}
\examples{
##  QR decomposition
A <- matrix(c(0,-4,2, 6,-3,-2, 8,1,-1), 3, 3, byrow=TRUE)
S <- householder(A)
(Q <- S$Q); (R <- S$R)
Q \%*\% R  # = A

##  Solve an overdetermined linear system of equations
A <- matrix(c(1:8,7,4,2,3,4,2,2), ncol=3, byrow=TRUE)
S <- householder(A); Q <- S$Q; R <- S$R
m <- nrow(A); n <- ncol(A)
b <- rep(6, 5)

x <- numeric(n)
b <- t(Q) \%*\% b
x[n] <- b[n] / R[n, n]
for (k in (n-1):1)
    x[k] <- (b[k] - R[k, (k+1):n] \%*\% x[(k+1):n]) / R[k, k]
qr.solve(A, rep(6, 5)); x
}
\keyword{ array }
