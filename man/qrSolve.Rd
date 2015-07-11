\name{qrSolve}
\alias{qrSolve}
\title{LSE Solution}
\description{
  Systems of linear equations via QR decomposition.
}
\usage{
  qrSolve(A, b)
}
\arguments{
  \item{A}{numerical matrix with \code{nrow(A)>=ncol(A)}.}
  \item{b}{numerical vector with \code{length(b) == nrow(A)}.}
}
\details{
  Solves (overdetermined) systems of linear equations via QR decomposition. 
}
\value{
  The solution of the system as vector.
}
\references{
  Trefethen, L. N., and D. Bau III. (1997). Numerical Linear Algebra. SIAM,
  Society for Industrial and Applied Mathematics, Philadelphia.
}
\seealso{
  \code{\link{householder}}
}
\examples{
A <- matrix(c(0,-4,2, 6,-3,-2, 8,1,-1), 3, 3, byrow=TRUE)
b <- c(-2, -6, 7)
qrSolve(A, b)

##  Solve an overdetermined linear system of equations
A <- matrix(c(1:8,7,4,2,3,4,2,2), ncol=3, byrow=TRUE)
b <- rep(6, 5)
x <- qrSolve(A, b)
qr.solve(A, rep(6, 5)); x
}
\keyword{ array }
