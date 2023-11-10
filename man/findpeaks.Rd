\name{findpeaks}
\alias{findpeaks}
\title{
Find Peaks
}
\description{
  Find peaks (maxima) in a time series.
}
\usage{
findpeaks(x, nups = 1, ndowns = nups, zero = "0", peakpat = NULL,
          minpeakheight = -Inf, minpeakdistance = 1,
          threshold = 0, npeaks = 0, sortstr = FALSE)
}
\arguments{
  \item{x}{numerical vector taken as a time series (no NAs allowed)}
  \item{nups}{minimum number of increasing steps before a peak is reached}
  \item{ndowns}{minimum number of decreasing steps after the peak}
  \item{zero}{can be `+', `-', or `0'; how to interprete succeeding steps
              of the same value: increasing, decreasing, or special}
  \item{peakpat}{define a peak as a regular pattern, such as the default
        pattern \code{[+]{1,}[-]{1,}}; if a pattern is provided, parameters
        \code{nups} and \code{ndowns} are not taken into account}
  \item{minpeakheight}{the minimum (absolute) height a peak has to have
        to be recognized as such}
  \item{minpeakdistance}{the minimum distance (in indices) peaks have to have
        to be counted}
  \item{threshold}{the minimum }
  \item{npeaks}{the number of peaks to return}
  \item{sortstr}{logical; should the peaks be returned sorted in decreasing
        oreder of their maximum value}
}
\details{
  This function is quite general as it relies on regular patterns to determine
  where a peak is located, from beginning to end.
}
\value{
  Returns a matrix where each row represents one peak found. The first column
  gives the height, the second the position/index where the maximum is reached,
  the third and forth the indices of where the peak begins and ends --- in the
  sense of where the pattern starts and ends.
}
\note{
  On Matlab Central there are several realizations for finding peaks, for
  example ``peakfinder'', ``peakseek'', or ``peakdetect''. And ``findpeaks''
  is also the name of a function in the Matlab `signal' toolbox.

  The parameter names are taken from the ``findpeaks'' function in `signal',
  but the implementation utilizing regular expressions is unique and fast.
}
\seealso{
  \code{\link{hampel}}
}
\examples{
x <- seq(0, 1, len = 1024)
pos <- c(0.1, 0.13, 0.15, 0.23, 0.25, 0.40, 0.44, 0.65, 0.76, 0.78, 0.81)
hgt <- c(4, 5, 3, 4, 5, 4.2, 2.1, 4.3, 3.1, 5.1, 4.2)
wdt <- c(0.005, 0.005, 0.006, 0.01, 0.01, 0.03, 0.01, 0.01, 0.005, 0.008, 0.005)

pSignal <- numeric(length(x))
for (i in seq(along=pos)) {
	pSignal <- pSignal + hgt[i]/(1 + abs((x - pos[i])/wdt[i]))^4
}
findpeaks(pSignal, npeaks=3, threshold=4, sortstr=TRUE)

\dontrun{
plot(pSignal, type="l", col="navy")
grid()
x <- findpeaks(pSignal, npeaks=3, threshold=4, sortstr=TRUE)
points(x[, 2], x[, 1], pch=20, col="maroon")}
}
\keyword{ timeseries }
