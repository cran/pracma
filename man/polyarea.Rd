\name{polyarea}
\alias{polyarea}
\alias{poly_center}
\alias{poly_length}
\alias{poly_crossings}
\title{Area of a Polygon}
\description{
  Calculates the area and length of a polygon given by the vertices in the 
  vectors \code{x} and \code{y}.
}
\usage{
  polyarea(x, y)

  poly_length(x, y)
  poly_center(x, y)

  poly_crossings(L1, L2)
}
\arguments{
  \item{x}{x-coordinates of the vertices defining the polygon}
  \item{y}{y-coordinates of the vertices}
  \item{L1, L2}{matrices of type \code{2xn} with x- and y-coordinates.}
}
\details{
  \code{polyarea} calculates the area of a polygon defined by the vertices
  with coordinates \code{x} and \code{y}. Areas to the left of the vertices
  are positive, those to the right are counted negative.

  The computation is based on the Gauss polygon area formula. The polygon
  automatically be closed, that is the last point need not be / should not
  be the same as the first.

  If some points of self-intersection of the polygon line are not in the
  vertex set, the calculation will be inexact. The sum of all areas will be
  returned, parts that are circulated in the mathematically negative sense
  will be counted as negative in this sum.

  If \code{x}, \code{y} are matrices of the same size, the areas of all
  polygons defined by corresponding columns are computed.

  \code{poly_center} calculates the center (of mass) of the figure defined by
  the polygon. Self-intersections should be avoided in this case.
  The mathematical orientation of the polygon does not have influence on the
  center coordinates.

  \code{poly_length} calculates the length of the polygon

  \code{poly_crossings} calculates the crossing points of two polygons given
  as matrices with x- and y-coordinates in the first and second row. Can be 
  used for finding the crossing points of parametrizised curves.
}
\value{
  Area or length of the polygon resp. sum of the enclosed areas; or the
  coordinates of the center of gravity.

  \code{poly_crossings} returns a matrix with column names \code{x} and
  \code{y} representing the crossing points.
}
\seealso{
  \code{\link{trapz}}, \code{\link{arclength}}
}
\examples{
  # Zu Chongzhi's calculation of pi (China, about 480 A.D.),
  # approximating the circle from inside by a regular 12288-polygon(!):
  phi <- seq(0, 2*pi, len=3*2^12+1)
  x <- cos(phi)
  y <- sin(phi)
  pi_approx <- polyarea(x, y)
  print(pi_approx, digits=8)    #=> 3.1415925 or 355/113

  poly_length(x, y)              #=> 6.2831852 where 2*pi is 6.2831853

  x1 <- x + 0.5; y1 <- y + 0.5
  x2 <- rev(x1); y2 <- rev(y1)
  poly_center(x1, y1)            #=> 0.5 0.5
  poly_center(x2, y2)            #=> 0.5 0.5

  # A simple example
  L1 <- matrix(c(0, 0.5, 1, 1,   2,
                0, 1,   1, 0.5, 0), nrow = 2, byrow = TRUE)
  L2 <- matrix(c(0.5, 0.75, 1.25, 1.25,
                0,   0.75, 0.75, 0   ), nrow = 2, byrow = TRUE)
  P <- poly_crossings(L1, L2)
  P
  ##         x     y
  ## [1,] 1.00 0.750
  ## [2,] 1.25 0.375

\dontrun{
  # Crossings of Logarithmic and Archimedian spirals
  # Logarithmic spiral
  a <- 1; b <- 0.1
  t <- seq(0, 5*pi, length.out = 200)
  xl <- a*exp(b*t)*cos(t) - 1
  yl <- a*exp(b*t)*sin(t)
  plot(xl, yl, type = "l", lwd = 2, col = "blue",
       xlim = c(-6, 3), ylim = c(-3, 4), xlab = "", ylab = "",
       main = "Intersecting Logarithmic and Archimedian spirals")
  grid()

  # Archimedian spiral
  a <- 0; b <- 0.25
  r <- a + b*t
  xa <- r * cos(t)
  ya <- r*sin(t)
  lines(xa, ya, type = "l", lwd = 2, col = "red")
  legend(-6.2, -1.0, c("Logarithmic", "Archimedian"),
         lwd = 2, col = c("blue", "red"), bg = "whitesmoke")

  L1 <- rbind(xl, yl)
  L2 <- rbind(xa, ya)
  P <- poly_crossings(L1, L2)
  points(P)
  }
}
\keyword{ math }
