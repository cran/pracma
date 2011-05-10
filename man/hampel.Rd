\name{hampel}
\alias{hampel}
\title{
Hampel Filter
}
\description{
Median absolute deviation (MAD) outlier in Time Series
}
\usage{
hampel(x, k)
}
\arguments{
  \item{x}{numeric vector representing a time series}
  \item{k}{window length \code{2*k+1} in indices}
}
\details{
  The `median absolute deviation' computation is done in the \code{[-k...k]}
  vicinity of each point at least \code{k} steps away from the end points of
  the interval.
}
\value{
  Returning a list \code{L} with \code{L$y} the corrected time series and
  \code{L$ind} the indices of outliers in the `median absolut deviation'
  sense.
}
\author{
  HwB  <hwborchers@googlemail.com>
}
\note{
  Don't take the expression \emph{outlier} too serious. It's just a hint to
  values in the time serious the appear to be unusual in the vicinity of
  their neighbors under a normal distribution assumption.
}
\references{
  Pearson, R. K. (1999). ``Data cleaning for dynamic modeling and control''.
  European Control Conference, ETH Zurich, Switzerland.
}
\seealso{
\code{\link{findpeaks}}
}
\examples{
set.seed(8421)
x <- numeric(1024)
z <- rnorm(1024)
x[1] <- z[1]
for (i in 2:1024) {
	x[i] <- 0.4*x[i-1] + 0.8*x[i-1]*z[i-1] + z[i]
}
omad <- hampel(x, k=20)

\dontrun{
plot(1:1024, x, type="l")
points(omad$ind, x[omad$ind], pch=21, col="darkred")}
}
\keyword{ timeseries }
