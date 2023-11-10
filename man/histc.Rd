\name{histc}
\alias{histc}
\title{
  Histogram Count (Matlab style)
}
\description{
  Histogram-like counting.
}
\usage{
  histc(x, edges)
}
\arguments{
  \item{x}{numeric vector or matrix.}
  \item{edges}{numeric vector of grid points, must be monotonically
        non-decreasing.}
}
\details{
  \code{n = histc(x,edges)} counts the number of values in vector \code{x}
  that fall between the elements in the \code{edges} vector (which must
  contain monotonically nondecreasing values).
  \code{n} is a \code{length(edges)} vector containing these counts.

  If \code{x} is a matrix then \code{cnt} and \code{bin} are matrices too, and

  \code{ for (j in (1:n))  cnt[k,j] <- sum(bin[, j] == k) }
}
\value{
  returns a list with components \code{cnt} and \code{bin}.
  \code{n(k)} counts the number of values in \code{x} that lie between
  \code{edges(k) <= x(i) < edges(k+1)}. The last counts any values of \code{x}
  that match \code{edges(n)}. Values outside the values in edges are not
  counted. Use \code{-Inf} and \code{Inf} in edges to include all values.

  \code{bin[i]} returns \code{k} if \code{edges(k) <= x(i) < edges(k+1)},
  and \code{0} if \code{x[i]} lies outside the grid.
}
\seealso{
  \code{\link{hist}}, \code{\link{histss}}, \code{\link{findInterval}}
}
\examples{
x <- seq(0.0, 1.0, by = 0.05)
e <- seq(0.1, 0.9, by = 0.10)
histc(x, e)
# $cnt
# [1] 2 2 2 2 2 2 2 2 1
# $bin
# [1] 0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 0 0

\dontrun{
# Compare
findInterval(x, e)
# [1] 0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 9
findInterval(x, e, all.inside = TRUE)
# [1] 1 1 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 8 8 8
# cnt[i] <- sum(findInterval(x, e) == i)}

x <- matrix( c(0.5029, 0.2375, 0.2243, 0.8495,
               0.0532, 0.1644, 0.4215, 0.4135,
               0.7854, 0.0879, 0.1221, 0.6170), 3, 4, byrow = TRUE)
e <- seq(0.0, 1.0, by = 0.2)
histc(x, e)
# $cnt
#      [,1] [,2] [,3] [,4]
# [1,]    1    2    1    0
# [2,]    0    1    1    0
# [3,]    1    0    1    1
# [4,]    1    0    0    1
# [5,]    0    0    0    1
# [6,]    0    0    0    0
# 
# $bin
#      [,1] [,2] [,3] [,4]
# [1,]    3    2    2    5
# [2,]    1    1    3    3
# [3,]    4    1    1    4
}
\keyword{ manip }
