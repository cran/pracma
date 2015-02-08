\name{wilkinson}
\alias{wilkinson}
\title{wilkinson Matrix}
\description{
Generate the Wilkinson matrix of size \code{n x n}.The Wilkinson matrix for
testing eigenvalue computations
}
\usage{
wilkinson(n)
}
\arguments{
  \item{n}{integer}
}
\details{
The Wilkinson matrix for testing eigenvalue computations is a symmetric
matrix with three non-zero diagonals and with several pairs of nearly equal
eigenvalues. 
}
\value{
matrix of size \code{n x n}
}
\note{
The two largest eigenvalues of \code{wilkinson(21)} agree to 14, but not 15
decimal places.
}
\seealso{
\code{\link{Toeplitz}}
}
\examples{
(a <- wilkinson(7))
eig(a)
}
\keyword{ specmat }
