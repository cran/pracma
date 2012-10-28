\name{trapz}
\alias{trapz}
\title{Trapezoidal Integration}
\description{
  Compute the area of a function with values \code{y} at the points
  \code{x}.
}
\usage{
  trapz(x, y)
}
\arguments{
  \item{x}{x-coordinates of points on the x-axis}
  \item{y}{y-coordinates of function values}
}
\details{
  The points \code{(x, 0)} and \code{(x, y)} are taken as vertices of a
  polygon and the area is computed using \code{polyarea}.

  This approach matches exactly the approximation for integrating the
  function using the trapezoidal rule with basepoints \code{x}.
}
\value{
  Approximated integral of the function from \code{min(x)} to \code{max(x)}.
}
\seealso{
  \code{\link{polyarea}}
}
\examples{
  # Calculate the area under the sine curve from 0 to pi:
  x <- seq(0, pi, len=101)
  y <- sin(x)
  trapz(x, y)  #=> 1.999836
}
\keyword{ math }
