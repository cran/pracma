\name{Trace}
\alias{Trace}
\title{Matrix trace}
\description{
Sum of the main diagonal elements.
}
\usage{
Trace(a)
}
\arguments{
  \item{a}{a square matrix}
}
\details{
Sums the elements of the main diagonal of areal or complrx square matrix.
}
\value{
scalar value
}
\note{
The corresponding function in Matlab/Octave is called trace(), but this in \R
has a different meaning.
}
\seealso{
\code{\link{Diag}}, \code{\link{diag}}
}
\examples{
Trace(matrix(1:16, nrow=4, ncol=4))
}
\keyword{ array }
