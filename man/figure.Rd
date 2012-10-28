\name{figure}
\alias{figure}
\title{
  Control Plot Devices (Matlab Style)
}
\description{
  Open, activate, and close grahics devices.
}
\usage{
figure(figno, title = "")
}
\arguments{
  \item{figno}{(single) number of plot device.}
  \item{title}{title of the plot device; not yet used.}
}
\details{
  The number of a graphics device cannot be 0 or 1. The function will work
  for the operating systems Mac OS, MS Windows, and most Linux systems.
  
  If \code{figno} is negative and a graphics device with that number does
  exist, it will be closed.
}
\value{
  No return value, except when a device of that number does not exist,
  in which case it returns a list of numbers of open graphics devices.
}
\note{
  Does not bring the activated graphics device in front.
}
\seealso{
  \code{dev.set, dev.off, dev.list}
}
\examples{
\dontrun{
figure()
figure(-2)
}
}
\keyword{ graphs }
