\name{ezplot}
\alias{ezplot}
\title{
  Easy Function Plot
}
\description{
  Easy function plot/wo the need to define \code{x, y} coordinates.
}
\usage{
ezplot(f, a, b, n = 101, col = "blue",
       grid = TRUE, gridcol = "gray", fill = FALSE, fillcol = "lightgray",
       xlab = "x", ylab = "f (x)", main = "Function Plot", ...)
}
\arguments{
  \item{f}{Function to be plotted.}
  \item{a, b}{Left and right endpoint for the plot.}
  \item{n}{Number of points to plot.}
  \item{col}{Color of the function graph.}
  \item{grid}{Logical; shall a grid be plotted?; default \code{TRUE}.}
  \item{gridcol}{Color of grid points.}
  \item{fill}{Logical; shall the area between function and axis be filled?;
              default: \code{FALSE}.}
  \item{fillcol}{Color of fill area.}
  \item{xlab}{Label on the \code{x}-axis.}
  \item{ylab}{Label on the \code{y}-axis.}
  \item{main}{Title of the plot}
  \item{...}{More parameters to be passed to \code{plot}.}
}
\details{
  Calculates the \code{x, y} coordinates of points to be plotted and
  calls the \code{plot} function.

  If \code{fill} is \code{TRUE}, also calls the \code{polygon} function
  with the \code{x, y} coordinates in appropriate order.
}
\value{
  Plots the function graph and invisibly returns \code{NULL}.
}
\note{
  Should mimick the Matlab function of the same name, has some more
  functionality, misses the possibility to plot several functions.
}
\seealso{
  \code{\link{curve}}
}
\examples{
\dontrun{
fun <- function(x) x * cos(0.1*exp(x)) * sin(0.1*pi*exp(x))
ezplot(fun, 0, 5, n = 1001, fill = TRUE)
  }
}
\keyword{ graphs }
