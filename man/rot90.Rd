\name{rot90}
\alias{rot90}
\title{Matrix Rotation}
\description{
Rotate matrices for 90, 180, or 270 degrees..
}
\usage{
rot90(a, k = 1)
}
\arguments{
  \item{a}{numeric or complex matrix}
  \item{k}{scalar integer number of times the matrix will be rotated for
  90 degrees; may be negative.}
}
\details{
  Rotates a numeric or complex matrix for 90 (k = 1), 180 (k = 2) or
  270 (k = 3 or k = -1) degrees.

  Value of k is taken mod 4.
}
\value{
the original matrix rotated
}
\examples{
a <- matrix(1:12, nrow=3, ncol=4, byrow=TRUE)
rot90(a)
rot90(a, 2)
rot90(a, -1)
}
\keyword{ manip }
