\name{Hadamard}
\alias{Hadamard}
\title{Hadamard Matrix}
\description{
  Generate Hadamard matrix of a certain size.
}
\usage{
Hadamard(n)
}
\arguments{
  \item{n}{An integer of the form 2^e, 12*2^e, or 20*2^e}
}
\details{
  An \code{n}-by-\code{n} Hadamard matrix with \code{n>2} exists only if
  \code{rem(n,4)=0}. This function handles only the cases where \code{n},
  \code{n/12}, or \code{n/20} is a power of 2.
}
\value{
  Matrix of size \code{n}-by-\code{n} of orthogonal columns consisting of
  1 and -1 only.
}
\note{
  Hadamard matrices have applications in combinatorics, signal processing,
  and numerical analysis.
}
\seealso{
  \code{\link{hankel}}, \code{\link{Toeplitz}}
}
\examples{
Hadamard(4)
H <- Hadamard(8)
t(H) %*% H        # t(H) %*% H == diag(n, n, n)
}
\keyword{ array }
