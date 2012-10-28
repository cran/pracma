\name{plotyy}
\alias{plotyy}
\title{
  Plotting Two y-Axes
}
\description{
  Line plot with y-axes on both left and right side.
}
\usage{
plotyy(x1, y1, x2, y2, gridp = TRUE, box.col = "grey",
                       type = "l", lwd = 1, lty = 1,
                       xlab = "x", ylab = "y", main = "",
                       col.y1 = "navy", col.y2 = "maroon", ...)
}
\arguments{
    \item{x1, x2}{x-coordinates for the curves}
    \item{y1, y2}{the y-values, with ordinates y1 left, y2 right.}
    \item{gridp}{logical; shall a grid be plotted.}
    \item{box.col}{color of surrounding box.}
    \item{type}{type of the curves, line or points (for both data).}
    \item{lwd}{line width (for both data).}
    \item{lty}{line type (for both data).}
    \item{xlab, ylab}{text below and on the left.}
    \item{main}{main title of the plot.}
    \item{col.y1, col.y2}{colors to be used for the lines or points.}
    \item{...}{additional plotting parameters.}
}
\details{
  Plots \code{y1} versus \code{x1} with y-axis labeling on the left and plots
  \code{y2} versus \code{x2} with y-axis labeling on the right.

  The x-values should not be too far appart. To exclude certain points, use
  \code{NA} values. Both curves will be line or point plots, and have the
  same line type and width.
}
\value{
  Generates a graph, no return values.
}
\seealso{
  \code{plotrix::twoord.plot}
}
\examples{
\dontrun{
x  <- seq(0, 20, by = 0.01)
y1 <- 200*exp(-0.05*x)*sin(x)
y2 <- 0.8*exp(-0.5*x)*sin(10*x)

plotyy(x, y1, x, y2, main = "Two-ordinates Plot")
}
}
\keyword{ graphs }
