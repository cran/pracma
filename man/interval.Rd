\name{interv_union}
\alias{interv_union}
\alias{interv_intersect}
\title{
  Interval Functions
}
\description{
  Functions for determining the union or intersection of a set of (closed)
  intervals of the real number line.
}
\usage{
interv_union(M)
interv_intersect(M)
}
\arguments{
  \item{M}{matrix representing a set of intervals.}
}
\details{
  Intersection is easy, as it is the interval from the maximum of left end
  points to the minimum of the right end points (or NULL if the maximum is
  greater than the minimum).

  NEW: Vectorized according to an idea of William Dunlap on R-help 01-2012.\cr
  OLD: For the union, the intervals are first ordered by left endpoints,
  then a simple loop checking intersection of two adjacent intervals.
}
\value{
  Another matrix representing a set of intervals.
}
\author{
  HwB  email: <hwborchers@googlemail.com>
}
\examples{
set.seed(8237)
x <- sample(1:100, 10)
M <- cbind(x, x+10)

interv_union(M)
interv_intersect(M)
}
\keyword{ manip }
