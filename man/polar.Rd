\name{polar}
\alias{polar}
\title{
  Random Points in Unit Circle (Matlab Style)
}
\description{
  Generate uniformly random points in the unit circle (or in a circle
  of radius r).
}
\usage{
polar(n = 1, r = 1)
}
\arguments{
  \item{n}{number of points, default 1.}
  \item{r}{radius of circle, default 1.}
}
\details{
  Generates tuples of points and rejects those outside the circle.
}
\value{
  A pair of values representing a point in the circle, or a matrix of size
  (n, 2).
}
\author{
  HwB  email: <hwborchers@googlemail.com>
}
\examples{
U <- polar(1000, 2)
\dontrun{
plot(U[, 1], U[, 2], pch="+")}

#-- v is 2 independent normally distributed elements
# u <- polar(1); r <- t(u) %*% u
# v <- sqrt(-2 * log(r)/r) * u

n <- 5000; U <- polar(n)
R <- apply(U*U, 1, sum)
P <- sqrt(-2 * log(R)/R) * U  # rnorm(2*n)
\dontrun{
hist(c(P))}
}
\keyword{ stat }
