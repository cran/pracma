\name{polyfit}
\alias{polyfit}
\alias{polyfit2}
\title{Fitting by Polynomial}
\description{
  Polynomial curve fitting
}
\usage{
polyfit(x, y, n)

polyfit2(x, y, n = 1, p0 = NULL)  
}
 \arguments{
  \item{x}{x-coordinates of points}
  \item{y}{y-coordinates of points}
  \item{n}{degree of the fitting polynomial}
  \item{p0}{Point \code{p0 = [x0, y0]} }
}
\details{
  \code{polyfit} finds the coefficients of a polynomial of degree \code{n}
  fitting the points given by their \code{x}, \code{y} coordinates in a
  least-squares sense.

  \code{polyfit2} finds a polynomial that fits the data in a least-squares
  sense, but also passes through \code{y0} for \code{x=x0}.
  
  In \code{polyfit}, if \code{x}, \code{y} are matrices of the same size,
  the coordinates are taken elementwise. Complex values are not allowed.
}
\value{
  vector representing a polynomial
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
}
\keyword{ math }
