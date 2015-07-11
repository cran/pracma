\name{geomean, harmmean}
\alias{geomean}
\alias{harmmean}
\alias{trimmean}
\title{
  Geometric and Harmonic Mean (Matlab Style)
}
\description{
  Geometric and harmonic mean along a dimension of a vector, matrix, or
  array.\cr
  \code{trimmean} is almost the same as \code{mean} in R.
}
\usage{
geomean(x, dim = 1)
harmmean(x, dim = 1)

trimmean(x, percent = 0)
}
\arguments{
  \item{x}{numeric vector, matrix, or array.}
  \item{dim}{dimension along which to take the mean; \code{dim=1} means
             along columns, \code{dim=2} along rows, the result will still
             be a row vector, not a column vector as in Matlab.}
  \item{percent}{percentage, between 0 and 100, of trimmed values.}
}
\details{
  \code{trimmean} does not call \code{mean} with the \code{trim} option, but
  rather calculates \code{k<-round(n*percent/100/2)} and leaves out \code{k}
  values at the beginning and end of the sorted \code{x} vector (or row or
  column of a matrix).
}
\value{
  Returns a scalar or vector (or array) of geometric or harmonic means:
  For \code{dim=1} the mean of columns, \code{dim=2} the mean of rows, etc.
}
\note{
  To have an exact analogue of \code{mean(x)} in Matlab,
  apply \code{trimmean(x)}.
}
\seealso{
  \code{\link{mean}}
}
\examples{
A <- matrix(1:12, 3, 4)
geomean(A, dim = 1)
## [1]  1.817121  4.932424  7.958114 10.969613
harmmean(A, dim = 2)
## [1] 2.679426 4.367246 5.760000

x <- c(-0.98, -0.90, -0.68, -0.61, -0.61, -0.38, -0.37, -0.32, -0.20, -0.16,
        0.00,  0.05,  0.12,  0.30,  0.44,  0.77,  1.37,  1.64,  1.72,  2.80)
trimmean(x); trimmean(x, 20)    # 0.2  0.085
mean(x); mean(x, 0.10)          # 0.2  0.085
}
\keyword{ stat }
