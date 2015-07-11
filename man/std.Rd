\name{std}
\alias{std}
\title{Standard Deviation (Matlab Style)}
\description{
  Standard deviation of the values of \code{x}.
}
\usage{
std(x, flag=0)
}
\arguments{
  \item{x}{numeric vector or matrix}
  \item{flag}{numeric scalar. If \code{0}, selects unbiased algorithm; and
  if \code{1}, selects the biased version.}
}
\details{
  If \code{flag = 0} the result is the square root of an unbiased estimator of 
  the variance. \code{std(X,1)} returns the standard deviation producing the
  second moment of the set of values about their mean.
}
\value{
  Return value depends on argument \code{x}. If vector, returns the
  standard deviation. If matrix, returns vector containing the standard
  deviation of each column.
}
\note{
  \code{flag = 0} produces the same result as R's sd().
}
\examples{
std(1:10)          # 3.027650
std(1:10, flag=1)  # 2.872281
}
\keyword{ stat }

