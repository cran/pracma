\name{dot}
\alias{dot}
\title{Scalar Product}
\description{
  'dot' or 'scalar' product of vectors or pairwise columns of matrices.
}
\usage{
  dot(x, y)
}
\arguments{
  \item{x}{numeric vector or matrix}
  \item{y}{numeric vector or matrix}
}
\details{
  Returns the 'dot' or 'scalar' product of vectors or columns of matrices.
  Two vectors must be of same length, two matrices must be of
  the same size.
  If \code{x} and \code{y} are column or row vectors, their dot product
  will be computed as if they were simple vectors.
}
\value{
  A scalar or vector of length the number of columns of \code{x} and
  \code{y}. 
}
\seealso{
  \code{\link{cross}}
}
\examples{
  dot(1:5, 1:5)  #=> 55
  # Length of space diagonal in 3-dim- cube:
  sqrt(dot(c(1,1,1), c(1,1,1)))  #=> 1.732051
}
\keyword{ array }
