\name{moler}
\alias{moler}
\title{Moler Matrix}
\description{
Generate the Moler matrix of size \code{n x n}. The Moler matrix is for
testing eigenvalue computations.
}
\usage{
moler(n)
}
\arguments{
  \item{n}{integer}
}
\details{
The Moler matrix for testing eigenvalue computations is a symmetric
matrix with exactly one small eigenvalue. 
}
\value{
matrix of size \code{n x n}
}
\seealso{
\code{\link{wilkinson}}
}
\examples{
(a <- moler(10))
min(eig(a))
}
\keyword{ specmat }
