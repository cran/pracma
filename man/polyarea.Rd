\name{polyarea}
\alias{polyarea}
\title{Area of a Polygon}
\description{
  Compute the area of the polygon given by the vertices in the vectors
  \code{x} and \code{y}.
}
\usage{
  polyarea(x, y)
}
\arguments{
  \item{x}{x-coordinates of the vertices defining the polygon}
  \item{y}{y-coordinates of the vertices}
}
\details{
  If \code{x}, \code{y} are matrices of the same size, the areas of all
  polygons defined by corresponding columns are computed. Areas to the left
  of the edges are positive, those to the right are counted negative.

  The absolute value of the sum of those areas will be returned. parts that
  are circulated in the mathematically negative sense will be counted as
  negative in this sum.
  
  If some points of self-intersection of the polygon line are not in the
  vertex set, the calculation will be inexact.

  The computation is based on the Gauss polygon area formula.
}
\value{
  Araea of the polygon as a positive number.
}
\author{
  hwb \email{hwborchers@googlemail.com}
}
\seealso{
  \code{\link{trapz}}
}
\examples{
  # Zu Chongzhi's calculation of pi (China, about 480 A.D.),
  # approximating the circle from inside by a regular 12288-polygon(!):
  phi <- seq(0, 2*pi, len=3*2^12+1)
  x <- cos(phi)
  y <- sin(phi)
  pi_approx <- polyarea(x, y)
  print(pi_approx, digits=8)  #=> 3.1415925 or 355/113
}
\keyword{ math }
