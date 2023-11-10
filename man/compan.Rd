\name{compan}
\alias{compan}
\title{Companion Matrix}
\description{
  Computes the companion matrix of a real or complex vector.
}
\usage{
  compan(p)
}
\arguments{
  \item{p}{vector representing a polynomial}
}
\details{
  Computes the companion matrix corresponding to the vector \code{p}
  with \code{-p[2:length(p)]/p[1]} as first row.

  The eigenvalues of this matrix are the roots of the polynomial.
}
\value{
  A square matrix of \code{length(p)-1} rows and columns
}
\seealso{
  \code{\link{roots}}
}
\examples{
  p <- c(1, 0, -7, 6)
  compan(p)
  # 0  7 -6
  # 1  0  0
  # 0  1  0
}
\keyword{ array }
