\name{flipdim}
\alias{flipdim}
\alias{flipud}
\alias{fliplr}
\alias{circshift}
\title{Matrix Flipping (Matlab Style)}
\description{
Flip matrices up and down or left and right; or circulating indices per
dimension.
}
\usage{
flipdim(a, dim)
flipud(a)
fliplr(a)
circshift(a, sz)
}
\arguments{
  \item{a}{numeric or complex matrix}
  \item{dim}{flipping dimension; can only be 1 (default) or 2}
  \item{sz}{integer vector of length 1 or 2.}
}
\details{
\code{flipdim} will flip a matrix along the \code{dim} dimension, where
\code{dim=1} means flipping rows, and \code{dim=2} flipping the columns.

\code{flipud} and \code{fliplr} are simply shortcuts for \code{flipdim(a, 1)}
resp. \code{flipdim(a, 2)}.

\code{circshift(a, sz)} circulates each dimension
(should be applicable to arrays).
}
\value{
the original matrix somehow flipped or circularly shifted.
}
\examples{
a <- matrix(1:12, nrow=3, ncol=4, byrow=TRUE)
flipud(a)
fliplr(a)

circshift(a, c(1, -1))
v <- 1:10
circshift(v, 5)
}
\keyword{ manip }
