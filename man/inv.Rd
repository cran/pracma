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
square matrix that is the inverse of \code{a}
}
\note{
\code{inv()} is the function name used in Matlab/Octave.
}
\seealso{
\code{\link{solve}}
}
\examples{
a <- hilb(6)
b <- inv(a)
}
\keyword{ array }
