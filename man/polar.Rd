\name{polar}
\alias{polar}
\title{
  Polar Coordinate Plot (Matlab Style)
}
\description{
  The polar function accepts polar coordinates, plots them in a Cartesian
  plane, and draws the polar grid on the plane.
}
\usage{
polar(t, r, type="l", 
      col = "blue", grcol = "darkgrey", bxcol = "black",
      main = "Polar Plot", add = FALSE, ...)
}
\arguments{
    \item{t, r}{vectors specifying angle and radius.}
    \item{type}{type of the plot, lines, points, or no plotting.}
    \item{col}{color of the graph.}
    \item{grcol, bxcol}{color of grid anf box around the plot.}
    \item{main}{plot title.}
    \item{add}{logical; if true, the graph will be plotted into the
               coordinate system of an existing plot.}
    \item{...}{plotting parameters to be passed to the \code{points} function.}
}
\details{
    \code{polar(theta,rho)} creates a polar coordinate plot of the angle
    \code{theta} versus the radius \code{rho}. \code{theta} is the angle
    from the x-axis to the radius vector specified in radians; \code{rho}
    is the length of the radius vector.
}
\value{
  Generates a plot; no returns.
}
\examples{
\dontrun{
t <- deg2rad(seq(0, 360, by = 2))
polar(t, cos(2*t), bxcol = "white", main = "Sine and Cosine")
polar(t, sin(2*t), col = "red", add = TRUE)
}
}
\keyword{ graphs }
