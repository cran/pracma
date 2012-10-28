\name{barylag}
\alias{barylag}
\title{Barycentric Lagrange Interpolation}
\description{
  Barycentric Lagrange interpolation in one dimension.
}
\usage{
barylag(xi, yi, x)
}
\arguments{
  \item{xi, yi}{x- and y-coordinates of supporting nodes.}
  \item{x}{x-coordinates of interpolation points.}
}
\details{
  \code{barylag} interpolates the given data using the barycentric
  Lagrange interpolation formula (vectorized to remove all loops).
}
\value{
  Values of interpolated data at points \code{x}.
}
\references{
  Berrut, J.-P., and L. Nick Trefethen (2004). ``Barycentric Lagrange
  Interpolation''. SIAM Review, Vol. 46(3), pp.501--517.
}
\note{
  Barycentric interpolation is preferred because of its numerical stability.
}
\seealso{
Lagrange or Newton interpolation.
}
\examples{
##  Generates an example with plot.
# Input:
#   fun  ---  function that shall be 'approximated'
#   a, b ---  interval [a, b] to be used for the example
#   n    ---  number of supporting nodes
#   m    ---  number of interpolation points
# Output
#   plot of function, interpolation, and nodes
#   return value is NULL (invisible)
\dontrun{
barycentricExample <- function(fun, a, b, n, m)
{
	xi <- seq(a, b, len=n)
	yi <- fun(xi)
	x  <- seq(a, b, len=m)

	y <- barylag(xi, yi, x)
	plot(xi, yi, col="red", xlab="x", ylab="y",
		main="Example of barycentric interpolation")

	lines(x, fun(x), col="yellow", lwd=2)
	lines(x, y, col="darkred")

	grid()
}

barycentricExample(sin, -pi, pi, 11, 101)  # good interpolation
barycentricExample(runge, -1, 1, 21, 101)  # bad interpolation
}
}
\keyword{ math }
