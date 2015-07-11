\name{kron}
\alias{kron}
\title{Kronecker product (Matlab Style)}
\description{
Kronecker tensor product of two matrices.
}
\usage{
kron(a, b)
}
\arguments{
  \item{a}{real or complex matrix}
  \item{b}{real or complex matrix}
}
\details{
The Kronecker product is a large matrix formed by all products between the
elements of \code{a} and those of \code{b}. The first left block is a11*b,
etc.
}
\value{
an \code{(n*p x m*q}-matrix, if \code{a} is \code{(n x m} and
\code{b} is \code{(p x q)}.
}
\note{
\code{kron()} is an alias for the R function \code{kronecker()}, which can
also be executed with the binary operator `\%x\%'.
}
\examples{
a <- diag(1, 2, 2)
b <- matrix(1:4, 2, 2)
kron(a, b)
kron(b, a)
}
\keyword{ array }
