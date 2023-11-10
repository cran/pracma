\name{pascal}
\alias{pascal}
\title{
Pascal Triangle
}
\description{
  Pascal triangle in matrix format
}
\usage{
pascal(n, k = 0)
}
\arguments{
  \item{n}{natural number}
  \item{k}{natural number, \code{k <= n}}
}
\details{
  Pascal triangle with \code{k} variations.
}
\value{
  matrix representing the Pascal triangle
}
\seealso{
\code{nchoosek}
}
\examples{
pascal(5)
pascal(5, 1)
pascal(5, 2)
}
\keyword{ specmat }
