\name{find}
\alias{find}
\title{Find function (Matlab Style)}
\description{
  Finds indices of nonzero elements.
}
\usage{
find(v)
}
\arguments{
  \item{v}{logical or numeric vector or array}
}
\details{
  Finds indices of true or nonzero elements of argument \code{v};
  can be used with a logical expression.
}
\value{
  Indices of elements matching the expression \code{x}.
}
\examples{
find(-3:3 >= 0)
find(c(0, 1, 0, 2, 3))
}
\keyword{logic}

