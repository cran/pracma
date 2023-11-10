\name{segm_distance}
\alias{segm_distance}
\title{
  Segment Distance
}
\description{
  The minimum distance between a point and a segment, or the
  minimum distance between points of two segments.
}
\usage{
segm_distance(p1, p2, p3, p4 = c())
}
\arguments{
  \item{p1, p2}{end points of the first segment.}
  \item{p3, p4}{end points of the second segment, or the point \code{p3}
        alone if \code{p4} is \code{NULL}.}
}
\details{
  If \code{p4=c()}, determines the orthogonal line to the segment through
  the single point and computes the distance to the intersection point.

  Otherwise, it computes the distances of all four end points to the
  other segment and takes the minimum of those.
}
\value{
  Returns a list with component \code{l} the minimum distance and components
  \code{p, q} the two nearest points.

  If \code{p4=c()} then point \code{p} lies on the segment and \code{q} is
  \code{p4}.
}
\note{
  The interfaces of \code{segm_intersect} and \code{segm_distance} should be
  brought into line.
}
\seealso{
\code{\link{segm_intersect}}
}
\examples{
\dontrun{
plot(c(0, 1), c(0, 1), type = "n", asp=1, 
     xlab = "", ylab = "", main = "Segment Distances")
grid()
for (i in 1:20) {
    s1 <- matrix(runif(4), 2, 2)
    s2 <- matrix(runif(4), 2, 2)
    lines(s1[, 1], s1[, 2], col = "red")
    lines(s2[, 1], s2[, 2], col = "darkred")
    S <- segm_distance(s1[1,], s1[2,], s2[1,], s2[2,])
    S$d
    points(c(S$p[1], S$q[1]), c(S$p[2], S$q[2]), pch=20, col="navy")
    lines(c(S$p[1], S$q[1]), c(S$p[2], S$q[2]), col="gray")
}}
}
\keyword{ geom }
