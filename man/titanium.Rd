\name{titanium}
\alias{titanium}
\docType{data}
\title{
  Titanium Test Data
}
\description{
  The Titanium data set describes measurements of a certain property of 
  titanium as a function of temperature.
}
\usage{
data(titanium)
}
\format{
  The format is:\cr
  Two columns called `x' and `y', the first being the temperature.
}
\details{
  These data have become a standard test for data fitting since they are hard
  to fit by classical techniques and have a significant amount of noise.
}
\source{
  Boor, C. de, and J. R. Rice (1968). Least squares cubic spline
  approximation II -- Variable knots, CSD TR 21, Comp.Sci., Purdue Univ.
}
\examples{
\dontrun{
data(titanium)
plot(titanium)
grid()}
}
\keyword{ datasets }
