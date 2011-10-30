\name{segm_intersect}
\alias{segm_intersect}
\title{
  Segment Intersection
}
\description{
  Do two segments have at least one point in common?
}
\usage{
segm_intersect(s1, s2)
}
\arguments{
  \item{s1, s2}{Two segments, represented by their end points; i.e.,
        \code{s <- rbind(p1, p2)} when \code{p1, p2} are the end points.}
}
\details{
  First compares the `bounding boxes', and if those intersect looks at
  whether the other end points lie on different sides of each segment.
}
\value{
  Logical, \code{TRUE} if these segments intersect.
}
\references{
  Cormen, Th. H., Ch. E. Leiserson, and R. L. Rivest (2009). Introduction
  to Algorithms. Third Edition, The MIT Press, Cambridge, MA.
}
\note{
  Should be written without reference to the \code{cross} function.
  Should also return the intersection point, see the example.
}
\seealso{
\code{\link{segm_distance}}
}
\examples{
\dontrun{
plot(c(0, 1), c(0, 1), type="n",
     xlab = "", ylab = "", main = "Segment Intersection")
grid()
for (i in 1:20) {
s1 <- matrix(runif(4), 2, 2)
s2 <- matrix(runif(4), 2, 2)
if (segm_intersect(s1, s2)) {
    clr <- "red"
    p1 <- s1[1, ]; p2 <- s1[2, ]; p3 <- s2[1, ]; p4 <- s2[2, ]
    A <- cbind(p2 - p1, p4 - p3)
    b <- (p3 - p1)
    a <- solve(A, b)
    points((p1 + a[1]*(p2-p1))[1], (p1 + a[1]*(p2-p1))[2], pch = 19, col = "blue")
} else
    clr <- "darkred"
lines(s1[,1], s1[, 2], col = clr)
lines(s2[,1], s2[, 2], col = clr)
}}
}
\keyword{ geom }
