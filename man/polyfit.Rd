\name{polyfit}
\alias{polyfit}
\title{Fitting by Polynomial}
\description{
  Polynomial curve fitting
}
\usage{
  polyfit(x, y, n)
}
 \arguments{
  \item{x}{x-coordinates of points}
  \item{y}{y-coordinates of points}
  \item{n}{degree of the fitting polynomial}
}
\details{
  Calculates the coefficients of a polynomial of degree \code{n}
  fitting the points given by their \code{x}, \code{y} coordinates
  in a least-squares sense.
  
  if \code{x}, \code{y} are matrices of the same size, the coordinates
  are taken elementwise. Complex values are not allowed.
}
\value{
  vector representing a polynomial
}
\seealso{
  \code{\link{polydef}}, \code{\link{polyval}}
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
  grid()
}
}
\keyword{ math }
