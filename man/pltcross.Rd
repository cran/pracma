\name{pltcross}
\alias{pltcross}
\title{
  Plotting Crosses
}
\description{
  Plots crosses into an active graphics device.
}
\usage{
pltcross(x, y, cx = 0, cy = 0,
         clen = 0.1, cent = FALSE, color = "darkgrey", ...)
}
\arguments{
  \item{x, y}{x- and y-coordinates of points.}
  \item{cx, cy}{x- and y-coordinates of ceter points of crosses.}
  \item{clen}{length of crosses in proportion of displayed coordinates, i.e.
              the min and max values of \code{x} and \code{y}.}
  \item{cent}{logical; if true an additional point will be plotted at the
              center of the cross.}
  \item{color}{color to be used for the lines of the crosses.}
  \item{...}{additional plotting argument to be passed to the \code{lines}
             function.}
}
\details{
  Plots one or a series of crosses into the active graphics window without
  erasing the existing content.

  The range of \code{x} and \code{y} is assumed to represent the upper and
  lower values of the coordinate axes. The length of the lines of the cross
  is then computed as \code{clen*range} where \code{clen} has to have values
  between 0 and 1.

  If \code{cent} is true, points will have the same color as its crosses.
  The point shape is the usual circle and cannot be changed at the moment.
}
\value{
  No value returned.
}
\note{
  See the example for generating a nice (abstract) picture generated with
  this function.
}
\seealso{
  \code{\link{symbol}}
}
\examples{
\dontrun{
plot(c(0,1), c(0,1), type = "n")
xc <- runif(200); yc <- runif(200)
clen <- pmin(xc, 1-xc, yc, 1-yc)
clr <- 1:8
plot(xc, yc, col=clr)
pltcross(c(0,1), c(0,1), xc, yc, clen = clen, color = clr)
}
}
\keyword{ graphs }
