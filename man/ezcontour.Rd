\name{ezcontour,ezsurf,ezmesh}
\alias{ezcontour}
\alias{ezsurf}
\alias{ezmesh}
\title{
  Contour, Surface, and Mesh Plotter
}
\description{
  Easy-to-use contour and 3-D surface resp mesh plotter.
}
\usage{
ezcontour(f, xlim = c(-pi,pi), ylim = c(-pi,pi), 
          n = 60, filled = FALSE, col = NULL)

ezsurf(f, xlim = c(-pi, pi), ylim = c(-pi, pi),
       n = 60, ...)

ezmesh(f, xlim = c(-pi,pi), ylim = c(-pi,pi), 
       n = 60, ...)
}
\arguments{
  \item{f}{2-D function to be plotted, must accept \code{(x,y)} as a vector.}
  \item{xlim, ylim}{defines x- and y-ranges as intervals.}
  \item{n}{number of grid points in each direction.}
  \item{col}{colour of isolines lines, resp. the surface color.}
  \item{filled}{logical; shall the contour plot be }
  \item{...}{parameters to be passed to the \code{persp} function.}
}
\details{
  \code{ezcontour} generates a contour plot of the function \code{f} using 
  \code{contour} (and \code{image} if \code{filled=TRUE} is chosen).
  If \code{filled=TRUE} is chosen, \code{col} should be a color scheme,
  the default is \code{heat.colors(12)}.

  \code{ezsurf} resp. \code{ezmesh} generates a surface/mesh plot of the 
  function \code{f} using \code{persp}.

  The function \code{f} needs not be vectorized in any form.
}
\value{
  Plots the function graph and invisibly returns \code{NULL}.
}
\note{
  Mimicks Matlab functions of the same names; Matlab's \code{ezcontourf} can 
  be generated with \code{filled=TRUE}.
}
\seealso{
  \code{\link{contour}}, \code{\link{image}}, \code{\link{persp}}
}
\examples{
\dontrun{
f <- function(xy) {
    x <- xy[1]; y <- xy[2]
    3*(1-x)^2 * exp(-(x^2) - (y+1)^2) -
        10*(x/5 - x^3 - y^5) * exp(-x^2 - y^2) -
        1/3 * exp(-(x+1)^2 - y^2)
    }
ezcontour(f, col = "navy")
ezcontour(f, filled = TRUE)
ezmesh(f)
ezmesh(f, col="lightblue", theta = -15, phi = 30)
  }
}
\keyword{ graphs }
