\name{flipdim}
\alias{flipdim}
\alias{flipud}
\alias{fliplr}
\title{Matrix flipping}
\description{
Flip matrices up and down or left and right.
}
\usage{
flipdim(a, dim)
flipud(a)
fliplr(a)
}
\arguments{
  \item{a}{numeric or complex matrix}
  \item{dim}{flipping dimension; can only be 1 (default) or 2}
}
\details{
\code{flipdim} will flip a matrix along the \code{dim} dimension, where
\code{dim=1} means flipping rows, and \code{dim=2} flipping the columns.

\code{flipud} and \code{fliplr} are simply shortcuts for \code{flipdim(a, 1)}
resp. \code{flipdim(a, 2)}.
}
\value{
the original matrix somehow flipped
}
\examples{
a <- matrix(1:12, nrow=3, ncol=4, byrow=TRUE)
flipud(a)
fliplr(a)
}
\keyword{ manip }
