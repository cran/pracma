\name{quiver}
\alias{quiver}
\title{
  Quiver or Velocity Plot
}
\description{
  A quiver plot displays velocity vectors as arrows with components
  \code{(u,v)} at the points \code{(x,y)}.
}
\usage{
    quiver(x, y, u, v,
           scale = 0.05, angle = 10, length = 0.1, ...)
}
\arguments{
  \item{x, y}{x,y-coordinates of start points of the arrows.}
  \item{u, v}{x,y-coordinates of start points.}
  \item{scale}{scales the length of the arrows.}
  \item{angle}{angle between shaft and edge of the arrows.}
  \item{length}{length of the arrow edges.}
  \item{...}{more options presented to the \code{arrows} primitive.}
}
\details{
  The matrices \code{x, y, u, v} must all be the same size and contain
  corresponding position and velocity components.
  However, x and y can also be vectors.
}
\value{
  Opens a graph window and plots the velocity vectors.
}
\seealso{
  \code{\link{vectorfield}}, \code{\link{arrows}}
}
\keyword{ graphs }
