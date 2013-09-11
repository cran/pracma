\name{rectint}
\alias{rectint}
\title{
  Rectangle Intersection Areas
}
\description{
  Calculates the area of intersection of rectangles, specified by position
  vectors \code{x} and \code{y}.
}
\usage{
rectint(x, y)
}
\arguments{
  \item{x, y}{both vectors of length 4, or both matrices with 4 columns.}
}
\details{
  Rectangles are specified as position vectors, that is \code{c(x[1],x[2])}
  is the lower left corner, \code{x[3]} and \code{x[4]} are width and height
  of the rectangle. When \code{x} and \code{y} are matrices, each row is
  assumed to be a position vector specifying a rectangle.
}
\value{
  Returns a scalar if \code{x} and \code{y} are vectors. If \code{x} is
  a \code{n-by-4} and \code{y} a \code{m-by-4} matrix, then it returns
  a \code{n-by-m} matrix \code{R} with entry \code{(i,j)} being the area
  \code{rectint(x[i,], y[j,])}.
}
\seealso{
  \code{\link{polyarea}}
}
\examples{
x <- c(0.5, 0.5, 0.25, 1.00)
y <- c(0.3, 0.3, 0.35, 0.75)
rectint(x, y)
# [1] 0.0825
}
\keyword{ math }
