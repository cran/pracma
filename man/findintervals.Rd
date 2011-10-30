\name{findintervals}
\alias{findintervals}
\title{
  Find Interval Indices
}
\description{
  Find indices \code{i} in vector \code{xs} such that either \code{x=xs[i]}
  or such that \code{xs[i]<x<xs[i+1]} or \code{xs[i]>x>xs[i+1]}.
}
\usage{
findintervals(x, xs)
}
\arguments{
  \item{x}{single number.}
  \item{xs}{numeric vector, not necessarily sorted.}
}
\details{
  Contrary to \code{findInterval}, the vector \code{xs} in
  \code{findintervals} need not be sorted.
}
\value{
  Vector of indices in \code{1..length(xs)}.
  If none is found, returns \code{integer(0)}.
}
\note{
  If \code{x} is equal to the last element in \code{xs}, the index
  \code{length(xs)} will also be returned.
}
\examples{
xs <- zapsmall(sin(seq(0, 10*pi, len=100)))
findintervals(0, xs)
#   1  10  20  30  40  50  60  70  80  90 100
}
\keyword{ logic }

