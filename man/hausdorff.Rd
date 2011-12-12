\name{hausdorff_dist}
\alias{hausdorff_dist}
\title{Hausdorff Distance}
\description{
  Hausdorff distance (aka Hausdorff dimension)
}
\usage{
hausdorff_dist(P, Q)
}
\arguments{
  \item{P, Q}{numerical matrices, representing points in an m-dim. space.}
}
\details{
  Calculates the Hausdorff Distance between two sets of points, P and Q.
  Sets P and Q must be matrices with the same number of columns (dimensions).

  The `directional' Hausdorff distance (dhd) is defined as:

    dhd(P,Q) = max p in P [ min q in Q [ ||p-q|| ] ]

  Intuitively dhd finds the point p from the set P that is farthest from any
  point in Q and measures the distance from p to its nearest neighbor in Q.
  The Hausdorff Distance is defined as max(dhd(P,Q),dhd(Q,P)).
}
\value{
  A single scalar, the Hausdorff distance (dimension).
}
\references{
  Barnsley, M. (1993). Fractals Everywhere. Morgan Kaufmann, San Francisco.
}
\seealso{
\code{\link{distmat}}
}
\examples{
P <- matrix(c(1,1,2,2, 5,4,5,4), 4, 2)
Q <- matrix(c(4,4,5,5, 2,1,2,1), 4, 2)
hausdorff_dist(P, Q)    # 4.242641 = sqrt(sum((c(4,2)-c(1,5))^2))
}
\keyword{ math }
