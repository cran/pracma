\name{detrend}
\alias{detrend}
\title{
  Remove Linear Trends
}
\description{
  Removes the mean value or (piecewise) linear trend from a vector or
  from each column of a matrix.
}
\usage{
detrend(x, tt = 'linear', bp = c())
}
\arguments{
  \item{x}{vector or matrix, columns considered as the time series.}
  \item{tt}{trend type, `constant' or `linear', default is `linear'.}
  \item{bp}{break points, indices between 1 and \code{nrow(x)}.}
}
\details{
  \code{detrend} computes the least-squares fit of a straight line (or
  composite line for piecewise linear trends) to the data and subtracts the
  resulting function from the data.

  To obtain the equation of the straight-line fit, use \code{polyfit}.
}
\value{
  removes the mean or (piecewise) linear trend from \code{x} and returns it
  in \code{y=detrend(x)}, that is \code{x-y} \emph{is} the linear trend.
}
\note{
  Detrending is often used for FFT processing.
}
\seealso{
  \code{\link{polyfit}}
}
\examples{
t <- 1:9
x <- c(0, 2, 0, 4, 4, 4, 0, 2, 0)
x - detrend(x, 'constant')
x - detrend(x, 'linear')

y <- detrend(x, 'linear', 5)
\dontrun{
plot(t, x, col="blue")
lines(t, x - y, col="red")
grid()}
}
\keyword{ math }
