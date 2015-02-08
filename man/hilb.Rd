\name{hilb}
\alias{hilb}
\title{Hilbert Matrix}
\description{
  Generate Hilbert matrix of dimension n
}
\usage{
  hilb(n)
}
\arguments{
  \item{n}{positive integer specifying the dimension of the Hilbert matrix}
}
\details{
  Generate the Hilbert matrix \code{H} of dimension \code{n} with elements
  \code{H[i, j] = 1/(i+j-1)}.

  (Note: This matrix is ill-conditioned, see e.g. \code{det(hilb(6))}.)
}
\value{
  matrix of dimension n
}
\seealso{
  \code{\link{vander}}
}
\examples{
  hilb(5)
}
\keyword{ specmat }
