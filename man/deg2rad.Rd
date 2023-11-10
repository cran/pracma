\name{deg2rad}
\alias{deg2rad}
\alias{rad2deg}
\title{Degrees to Radians}
\description{
  Transforms between angles in degrees and radians.
}
\usage{
deg2rad(deg)
rad2deg(rad)
}
\arguments{
  \item{deg}{(array of) angles in degrees.}
  \item{rad}{(array of) angles in radians.}
}
\details{
  This is a simple calculation back and forth. Note that angles greater than
  360 degrees are allowed and will be returned. This may appear incorrect but
  follows a corresponding discussion on Matlab Central.
}
\value{
  The angle in degrees or radians.
}
\examples{
deg2rad(c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90))
rad2deg(seq(-pi/2, pi/2, length = 19))
}
\keyword{math}
