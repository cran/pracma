\name{sind,cosd,tand, etc.}
\alias{sind}
\alias{cosd}
\alias{tand}
\alias{cotd}
\alias{asind}
\alias{acosd}
\alias{atand}
\alias{acotd}
\alias{secd}
\alias{cscd}
\alias{asecd}
\alias{acscd}
\alias{atan2d}
\title{
  Trigonometric Functions in Degrees
}
\description{
  Trigonometric functions expecting input in degrees, not radians.
}
\usage{
sind(x)
cosd(x)
tand(x)
cotd(x)
asind(x)
acosd(x)
atand(x)
acotd(x)
secd(x)
cscd(x)
asecd(x)
acscd(x)
atan2d(x1, x2)
}
\arguments{
  \item{x, x1, x2}{numeric or complex scalars or vectors}
}
\details{
  The usual trigonometric functions with input values as scalar or vector
  in degrees. Note that \code{tan(x)} with fractional part does not return
  \code{NaN} as \code{tanpi(x)}, but is computed as \code{sind(x)/cosd(x)}.

  For \code{atan2d} the inputs \code{x1,x2} can be both degrees or radians,
  but don't mix! The result is in degrees, of course.
}
\value{
  Returns a scalar or vector of numeric values.
}
\note{
  These function names are available in Matlab, that is the reason
  they have been added to the `pracma' package.
}
\seealso{
  Other trigonometric functions in R.
}
\examples{
# sind(x) and cosd(x) are accurate for x which are multiples
# of 90 and 180 degrees, while tand(x) is problematic.

x <- seq(0, 720, by = 90)
sind(x)                     # 0  1  0 -1  0  1  0 -1  0
cosd(x)                     # 1  0 -1  0  1  0 -1  0  1
tand(x)                     # 0  Inf  0  -Inf  0  Inf  0  -Inf  0
cotd(x)                     # Inf  0  -Inf  0  Inf  0  -Inf  0  Inf

x <- seq(5, 85, by = 20)
asind(sind(x))              # 5 25 45 65 85
asecd(sec(x))
tand(x)                     # 0.08748866  0.46630766  1.00000000  ...
atan2d(1, 1)                # 45
}
\keyword{ math }
