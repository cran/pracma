\name{blkdiag}
\alias{blkdiag}
\title{
Block Diagonal Matrix
}
\description{
  Build a block diagonal matrix.
}
\usage{
blkdiag(...)
}
\arguments{
  \item{\dots}{sequence of non-empty, numeric matrices}
}
\details{
  Generate a block diagonal matrix from A, B, C, .... 
  All the arguments must be numeric and non-empty matrices.
}
\value{
  a numeric matrix
}
\note{
  Vectors as input have to be converted to matrices before. Note that
  \code{as.matrix(v)} with \code{v} a vector will generate a column vector;
  use \code{matrix(v, nrow=1)} if a row vector is intended.
}
\seealso{
\code{\link{Diag}}
}
\examples{
a1 <- matrix(c(1,2), 1)
a2 <- as.matrix(c(1,2))
blkdiag(a1, diag(1, 2, 2), a2)
}
\keyword{ array }
