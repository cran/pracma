\name{Mode}
\alias{Mode}
\title{
Mode function (Matlab style)
}
\description{
  Most frequent value in vector or matrix
}
\usage{
Mode(x)
}
\arguments{
  \item{x}{Real or complex vector or of factor levels.}
}
\details{
  Computes the `sample mode', i.e. the most frequently occurring value in x.

  Among values occurring equally frequently, \code{Mode()} chooses the
  smallest one (for a numeric vector), one with a smallest absolute value
  (for complex ones) or the first occurring value (for factor levels).

  A matrix will be changed to a vector.
}
\value{
  One element from x and of the same type. The number of occurrences will 
  not be returned.
}
\note{
  In Matlab/Octave an array dimension can be selected along which to find
  the mode value; this has not been realized here.

  Shadows the R function \code{mode} that returns essentially the type
  of an object.
}
\seealso{
  \code{\link{median}}
}
\examples{
x <- round(rnorm(1000), 2)
Mode(x)
}
\keyword{ manip }
