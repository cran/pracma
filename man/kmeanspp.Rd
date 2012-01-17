\name{kmeanspp}
\alias{kmeanspp}
\title{
  K-means++ Clustering
}
\description{
  kmeans++ clustering algorithm
}
\usage{
kmeanspp(X, k)
}
\arguments{
  \item{X}{numeric matrix of data.}
  \item{k}{the number of clusters.}
}
\details{
  \code{kmeanspp} applies a specific way of choosing the centers that will
  be passed to the classical \code{kmeans} routine. The first center will be
  chosen at random, the next ones will be selected with a probability
  proportional to the shortest distance to the closest center already
  chosen.
}
\value{
  Returns an `object' of class ``kmeans'', because \code{kmeans} will be
  called in the end.
}
\note{
  Please note that it is not clear whether the approach of kmeans++ is 
  really preferable to, e.g., kmeans with several restarts.
}
\author{
  HwB  email: <hwborchers@googlemail.com>
}
\references{
  Arthur, D., and S. Vassilvitskii (2006). "k-means++: The Advantages of
  Careful Seeding", Technical Report 2006-13, Stanford InfoLab.
}
\seealso{
  \code{\link{kmeans}}
}
\examples{
X <- rbind(matrix(rnorm(500, mean = 0,  sd = 0.3), ncol = 2),
           matrix(rnorm(500, mean = 1,  sd = 0.3), ncol = 2),
           matrix(rnorm(500, mean = -1, sd = 0.3), ncol = 2))
colnames(X) <- c("x", "y")
cl <- kmeanspp(X, 3)
\dontrun{
plot(X, col = cl$cluster)
points(cl$centers, col = 1:3)
grid()}
}
\keyword{ datamining }
