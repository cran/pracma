\name{savgol}
\alias{savgol}
\title{
Savitzky-Golay Smoothing
}
\description{
  Polynomial filtering method of Savitzky and Golay.
}
\usage{
savgol(T, fl, forder = 4, dorder = 0)
}
\arguments{
  \item{T}{Vector of signals to be filtered.}
  \item{fl}{Filter length (for instance fl = 51..151), has to be odd.}
  \item{forder}{Filter order (2 = quadratic filter, 4 = quartic).}
  \item{dorder}{Derivative order (0 = smoothing, 1 = first derivative, etc.).}
}
\details{
  Savitzky-Golay smoothing performs a local polynomial regression on a
  series of values which are treated as being equally spaced to determine
  the smoothed value for each point.
  Methods are also provided for calculating derivatives.
}
\value{
  Vector representing the smoothed time series.
}
\references{
  See Numerical Recipes, 1992, Chapter 14.8, for details.
}
\author{
  Peter Riegler implemented a Matlab version in 2001. Based on this,
  Hans W. Borchers did an implementation in R in 2003.
}
\note{
  For derivatives T2 has to be divided by the step size to the order\cr
  (and to be multiplied by k! --- the sign appears to be wrong).
}
\seealso{
\code{sav_gol} in package `RTisean' or \code{sgolayfilt} in package `signal'.
}
\examples{
\dontrun{
# *** Sinosoid test function ***
ts <- sin(2*pi*(1:1000)/200)
t1 <- ts + rnorm(1000)/10
t2 <- savgol(t1, 51)
plot(1:1000, t1)
lines(1:1000, ts, col="blue")
lines(1:1000, t2, col="red")}
}
\keyword{ timeseries }
