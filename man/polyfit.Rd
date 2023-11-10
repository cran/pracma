\name{polyfit,polyfix}
\alias{polyfit}
\alias{polyfix}
\title{Fitting by Polynomial}
\description{
  Polynomial curve fitting
}
\usage{
polyfit(x, y, n)

polyfix(x, y, n, xfix, yfix)
}
 \arguments{
  \item{x}{x-coordinates of points}
  \item{y}{y-coordinates of points}
  \item{n}{degree of the fitting polynomial}
  \item{xfix,yfix}{x- and y-coordinates of points to be fixed}
}
\details{
  \code{polyfit} finds the coefficients of a polynomial of degree \code{n}
  fitting the points given by their \code{x}, \code{y} coordinates in a
  least-squares sense. In \code{polyfit}, if \code{x}, \code{y} are matrices
  of the same size, the coordinates are taken elementwise. Complex values are 
  not allowed.

  \code{polyfix} finds a polynomial that fits the data in a least-squares
  sense, but also passes exactly through all the points with coordinates
  \code{xfix} and \code{yfix}. Degree \code{n} should be greater or equal
  to the number of fixed points, but not too big to avoid `singular matrix'
  or similar error messages
}
\value{
  vector representing a polynomial.
}
\note{
  Please not that \code{polyfit2} is has been removed since 1.9.3; please use
  \code{polyfix} instead.
}
\seealso{
  \code{\link{poly}}, \code{\link{polyval}}
}
\examples{
  # Fitting the sine function by a polynomial
  x <- seq(0, pi, length.out=25)
  y <- sin(x)
  p <- polyfit(x, y, 6)
  
\dontrun{
  # Plot sin and fitted polynomial
  plot(x, y, type="b")
  yf <- polyval(p, x)
  lines(x, yf, col="red")
  grid()}

\dontrun{
  n <- 3
  N <- 100
  x <- linspace(0, 2*pi, N); y = sin(x) + 0.1*rnorm(N)
  xfix <- c(0, 2*pi); yfix = c(0, 0)

  xs <- linspace(0, 2*pi); ys <- sin(xs)
  plot(xs, ys, type = 'l', col = "gray",
	   main = "Polynom Approximation of Degree 3")
  grid()
  points(x, y, pch='o', cex=0.5)
  points(xfix, yfix, col = "darkred")

  p0 <- polyfit(x, y, n)
  lines(xs, polyval(p0, xs), col = "blue")

  p1 <- polyfix(x, y, n, xfix, yfix)
  lines(xs, polyval(p1, xs), col = "red")

  legend(4, 1, c("sin", "polyfit", "polyfix"),
         col=c("gray", "blue", "red"), lty=c(1,1,1))}
}
\keyword{ math }
