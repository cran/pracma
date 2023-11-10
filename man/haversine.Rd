\name{haversine}
\alias{haversine}
\title{
  Haversine Formula
}
\description{
  Haversine formula to calculate the arc distance between two points
  on earth (i.e., along a great circle).
}
\usage{
haversine(loc1, loc2, R = 6371.0)
}
\arguments{
  \item{loc1, loc2}{Locations on earth; for format see Details.}
  \item{R}{Average earth radius R = 6371 km, can be changed on input.}
}
\details{
  The Haversine formula is more robust for the calculating the distance 
  as with the spherical cosine formula. The user may want to assume a 
  slightly different earth radius, so this can be provided as input.

  The location can be input in two different formats, as latitude and
  longitude in a character string, e.g. for Frankfurt airport as
  '50 02 00N, 08 34 14E', or as a numerical two-vector in degrees
  (not radians).

  Here for latitude 'N' and 'S' stand for North and South, and for
  longitude 'E' or 'W' stand for East and West. For the degrees format,
  South and West must be negative.

  These two formats can be mixed.
}
\value{
  Returns the distance in km.
}
\references{
  Entry 'Great_circle_distance' in Wikipedia.
}
\author{
  Hans W. Borchers
}
\seealso{
  Implementations of the Haversine formula can also be found in other
  R packages, e.g. 'geoPlot' or 'geosphere'.
}
\examples{
FRA = '50 02 00N, 08 34 14E'  # Frankfurt Airport
ORD = '41 58 43N, 87 54 17W'  # Chicago O'Hare Interntl. Airport
fra <- c(50+2/60, 8+34/60+14/3600)
ord <- c(41+58/60+43/3600, -(87+54/60+17/3600))

dis <- haversine(FRA, ORD)    # 6971.059 km
fprintf('Flight distance Frankfurt-Chicago is \%8.3f km.\n', dis)

dis <- haversine(fra, ord)
fprintf('Flight distance Frankfurt-Chicago is \%8.3f km.\n', dis)
}
\keyword{ geom }
