\name{ezpolar}
\alias{ezpolar}
\title{
  Easy Polar Plot
}
\description{
  Easy function plot w/o the need to define \code{x, y} coordinates.
}
\usage{
ezpolar(fun, interv = c(0, 2*pi))
}
\arguments{
  \item{fun}{function to be plotted.}
  \item{interv}{left and right endpoint for the plot.}
}
\details{
  Calculates the \code{x, y} coordinates of points to be plotted and
  calls the \code{polar} function.
}
\value{
  Plots the function graph and invisibly returns \code{NULL}.
}
\note{
  Mimick the Matlab function of the same name.
}
\seealso{
  \code{\link{ezplot}}
}
\examples{
\dontrun{
fun <- function(x) 1 + cos(x)
ezpolar(fun)
  }
}
\keyword{ graphs }
