\name{piecewise}
\alias{piecewise}
\title{
Piecewise Linear Function
}
\description{
  Compute zeros and area of a piecewise linear function.
}
\usage{
piecewise(x, y, abs = FALSE)
}
\arguments{
  \item{x, y}{x- and y-coordinates of points defining the piecewise linear function}
  \item{abs}{logical; shall the integral or the total area between the
             x-axis and the function be calculated}
}
\details{
  Compute zeros and integral resp. area of a piecewise linear function
  given by points with x and y as coordinates.
}
\value{
  Returns a list with the integral or area as first element and the vector
  as all zeroes as second.
}
\seealso{
\code{\link{trapz}}
}
\examples{
x <- c(0,  2, 3,  4, 5)
y <- c(2, -2, 0, -2, 0)
piecewise(x, y)
piecewise(x, y, abs=TRUE)
}
\keyword{ math }
