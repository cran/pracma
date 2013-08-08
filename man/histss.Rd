\name{histss}
\alias{histss}
\title{
  Histogram Bin-width Optimization
}
\description{
  Method for selecting the bin size of time histograms.
}
\usage{
histss(x, n = 100, plotting = FALSE)
}
\arguments{
  \item{x}{numeric vector or matrix.}
  \item{n}{maximum number of bins.}
  \item{plotting}{logical; shall a histogram be plotted.}
}
\details{
  Bin sizes of histograms are optimized in a way to best displays the
  underlying spike rate, for example in neurophysiological studies.
}
\value{
  Returns the same list as the \code{hist} function; the list is invisible
  if the histogram is plotted.
}
\references{
  Shimazaki H. and S. Shinomoto. A method for selecting the bin size of
  a time histogram. Neural Computation (2007) Vol. 19(6), 1503-1527
}
\seealso{
  \code{\link{hist}}, \code{\link{histc}}
}
\examples{
x <- sin(seq(0, pi/2, length.out = 200))
H <- histss(x, n = 50, plotting = FALSE)
\dontrun{
plot(H, col = "gainsboro")  # Compare with hist(x), or
hist(x, breaks = H$breaks)  # the same }
}
\keyword{ timeseries }
