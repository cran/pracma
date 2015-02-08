\name{rosser}
\alias{rosser}
\title{Rosser Matrix}
\description{
Generate the Rosser matrix.
}
\usage{
rosser()
}
\details{
This is a classic symmetric eigenvalue test problem.
It has a double eigenvalue, three nearly equal eigenvalues, dominant
eigenvalues of opposite sign, a zero eigenvalue, and a small, nonzero
eigenvalue.
}
\value{
matrix of size 8 x 8
}
\seealso{
  \code{\link{wilkinson}}
}
\examples{
rosser()
}
\keyword{ specmat }
