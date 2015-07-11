\name{inv}
\alias{inv}
\title{Matrix Inverse (Matlab Style)}
\description{
Invert a numeric or complex matrix.
}
\usage{
inv(a)
}
\arguments{
  \item{a}{real or complex square matrix}
}
\details{
Computes the matrix inverse by calling \code{solve(a)} and catching the error
if the matrix is nearly singular.
}
\value{
square matrix that is the inverse of \code{a}.
}
\note{
\code{inv()} is the function name used in Matlab/Octave.
}
\seealso{
\code{\link{solve}}
}
\examples{
A <- hilb(6)
B <- inv(A)
B
# Compute the inverse matrix through Cramer's rule:
n <- nrow(A)
detA <- det(A) 
b <- matrix(NA, nrow = n, ncol = n)
for (i in 1:n) {
    for (j in 1:n) {
        b[i, j] <- (-1)^(i+j) * det(A[-j, -i]) / detA
    }
}
b
}
\keyword{ array }
