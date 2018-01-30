
\name{poisson2disk}
\alias{poisson2disk}
\title{
  Poisson Disk Sampling
}
\description{
  Approximate Poisson disk distribution of points in a rectangle.
}
\usage{
poisson2disk(n, a = 1, b = 1, m = 10, info = TRUE)
}
\arguments{
  \item{n}{number of points to generate in a rectangle.}
  \item{a, b}{width and height of the rectangle}
  \item{m}{number of points to try in each step.}
  \item{info}{shall additional info be printed.}
}
\details{
  Realizes Mitchell's best-candidate algorithm for creating a Poisson disk
  distribution on a rectangle. Can be used for sampling, and will be more 
  appropriate in some sampling applications than uniform sampling or 
  grid-like sampling.

  With m = 1 uniform sampling will be generated.
}
\value{
  Returns the points as a matrix with two columns for x- and
  y-coordinates. Prints the minimal distance between points generated.
}
\references{
  A. Lagae and Ph. Dutre. A Comparison of Methods for Generating Poisson
  Disk Distributions. Computer Graphics Forum, Vol. 27(1), pp. 114-129,
  2008. URL: citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.192.5862
}
\note{
  Bridson's algorithm for Poisson disk sampling may be added later as an
  alternative. Also a variant that generates points in a circle.
}
\examples{
set.seed(1111)
P <- poisson2disk(n = 20, m = 10)
head(P)
##            [,1]       [,2]
## [1,] 0.46550264 0.41292487
## [2,] 0.13710541 0.98737065
## [3,] 0.96028255 0.83222920
## [4,] 0.06044078 0.09325431
## [5,] 0.78579426 0.09267546
## [6,] 0.49670274 0.99852771

# Plotting points
# plot(P, pch = 'x', col = "blue")
}
\keyword{ stat }
