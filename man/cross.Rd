\name{cross}
\alias{cross}
\title{Vector Cross Product}
\description{
  Vector or cross product
}
\usage{
  cross(x, y)
}
\arguments{
  \item{x}{numeric vector or matrix}
  \item{y}{numeric vector or matrix}
}
\details{
  Computes the cross (or: vector) product of vectors in 3 dimensions.
  In case of matrices it takes the first dimension of length 3 and
  computes the cross product between corresponding columns or rows.

  The more general cross product of \code{n-1} vectors in n-dimensional
  space is realized as \code{crossn}.
}
\value{
  3-dim. vector if \code{x} and \code{<} are vectors, a matrix of
  3-dim. vectors if \code{x} and \code{y} are matrices themselves.
}
\seealso{
  \code{\link{dot}}, \code{\link{crossn}}
}
\examples{
  cross(c(1, 2, 3), c(4, 5, 6))  # -3  6 -3
}
\keyword{ array }
