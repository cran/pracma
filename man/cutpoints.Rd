\name{cutpoints}
\alias{cutpoints}
\title{
  Find Cutting Points
}
\description{
  Finds cutting points for vector s of real numbers.
}
\usage{
cutpoints(x, nmax = 8, quant = 0.95)
}
\arguments{
  \item{x}{vector of real values.}
  \item{nmax}{the maximum number of cutting points to choose}
  \item{quant}{quantile of the gaps to consider for cuts.}
}
\details{
  Finds cutting points for vector s of real numbers, based on the gaps in
  the values of the vector. The number of cutting points is derived from a
  quantile of gaps in the values. The user can set a lower limit for this
  number of gaps.
}
\value{
  Returns a list with components \code{cutp}, the cutting points selected,
  and \code{cutd}, the gap between values of \code{x} at this cutting point.
}
\note{
  Automatically finding cutting points is often requested in Data Mining.
  If a target attribute is available, Quinlan's C5.0 does a very good job
  here. Unfortunately, the `C5.0' package (of the R-Forge project ``Rulebased
  Models'') is quite cumbersome to use.
}
\references{
  Witten, I. H., and E. Frank (2005). Data Mining: Practical Machine
  Learning Tools and Techniques. Morgan Kaufmann Publishers, San Francisco.
}
\seealso{
  \code{\link{cut}}
}
\examples{
N <- 100; x <- sort(runif(N))
cp <- cutpoints(x, 6, 0.9)
n <- length(cp$cutp)

# Print out
nocp <- rle(findInterval(x, c(-Inf, cp$cutp, Inf)))$lengths
cbind(c(-Inf, cp$cutp), c(cp$cutp, Inf), nocp)

# Define a factor from the cutting points
fx <- cut(x, breaks = c(-Inf, cp$cutp, Inf))

\dontrun{
# Plot points and cutting points
plot(x, rep(0, N), col="gray", ann = FALSE)
points(cp$cutp, rep(0, n), pch="|", col=2)

# Compare with k-means clustering
km <- kmeans(x, n)
points(x, rep(0, N), col = km$cluster, pch = "+")

##  A 2-dimensional example
x <- y <- c()
for (i in 1:9) {
  for (j in 1:9) {
    x <- c(x, i + rnorm(20, 0, 0.2))
    y <- c(y, j + rnorm(20, 0, 0.2))
  }
}
cpx <- cutpoints(x, 8, 0)
cpy <- cutpoints(y, 8, 0)

plot(x, y, pch = 18, col=rgb(0.5,0.5,0.5), axes=FALSE, ann=FALSE)
for (xi in cpx$cutp) abline(v=xi, col=2, lty=2)
for (yi in cpy$cutp) abline(h=yi, col=2, lty=2)

km <- kmeans(cbind(x, y), 81)
points(x, y, col=km$cluster)
}
}
\keyword{ timeseries }
