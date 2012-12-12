\name{sortrows}
\alias{sortrows}
\title{Sort Rows of a Matrix (Matlab Style)}
\description{
  Sort rows of a matrix according to values in a column.
}
\usage{
sortrows(A, k = 1)
}
\arguments{
  \item{A}{numeric matrix.}
  \item{k}{number of column to sort the matrix accordingly.}
}
\details{
  \code{sortrows(A, k)} sorts the rows of the matrix \code{A} such that
  column \code{k} is increasingly sorted.
}
\value{
  Returns the sorted matrix.
}
\seealso{ 
  \code{\link{sort}}
}   
\examples{
A <- magic(5)
sortrows(A)
sortrows(A, k = 2)
}
\keyword{array}
