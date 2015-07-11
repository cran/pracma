\name{inpolygon}
\alias{inpolygon}
\title{
  Polygon Region
}
\description{
  Points inside polygon region.
}
\usage{
inpolygon(x, y, xp, yp, boundary = FALSE)
}
\arguments{
  \item{x, y}{x-, y-coordinates of points to be tested for being inside
              the polygon region.}
  \item{xp, yp}{coordinates of the vertices specifying the polygon.}
  \item{boundary}{Logical; does the boundary belong to the interior.}
}
\details{
  For a polygon defined by points \code{(xp, yp)}, determine if the
  points \code{(x, y)} are inside or outside the polygon. The boundary
  can be included or excluded (default) for the interior.
}
\value{
  Logical vector, the same length as \code{x}.
}
\references{
  Hormann, K., and A. Agathos (2001). The Point in Polygon Problem for
  Arbitrary Polygons. Computational Geometry, Vol. 20, No. 3, pp. 131--144.
}
\note{
  Special care taken for points on the boundary.
}
\seealso{
\code{\link{polygon}}
}
\examples{
xp <- c(0.5, 0.75, 0.75, 0.5, 0.5)
yp <- c(0.5, 0.5, 0.75, 0.75, 0.5)
x <- c(0.6, 0.75, 0.6, 0.5)
y <- c(0.5, 0.6, 0.75, 0.6)
inpolygon(x, y, xp, yp, boundary = FALSE)  # FALSE
inpolygon(x, y, xp, yp, boundary = TRUE)   # TRUE

\dontrun{
pg <- matrix(c(0.15, 0.75, 0.25, 0.45, 0.70,
               0.80, 0.35, 0.55, 0.20, 0.90), 5, 2)
plot(c(0, 1), c(0, 1), type="n")
polygon(pg[,1], pg[,2])
P <- matrix(runif(20000), 10000, 2)
R <- inpolygon(P[, 1], P[, 2], pg[, 1], pg[,2])
clrs <- ifelse(R, "red", "blue")
points(P[, 1], P[, 2], pch = ".", col = clrs)}
}
\keyword{ geom }
