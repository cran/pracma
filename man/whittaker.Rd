\name{whittaker}
\alias{whittaker}
\title{
  Whittaker Smoothing
}
\description{
  Smoothing of time series using the Whittaker-Henderson approach.
}
\usage{
whittaker(y, lambda = 1600, d = 2)
}
\arguments{
  \item{y}{signal to be smoothed.}
  \item{lambda}{smoothing parameter (rough 50..1e4 smooth); the default
                value of 1600 has been recommended in the literature.}
  \item{d}{order of differences in penalty (generally 2)}
}
\details{
  The Whittaker smoother family was first presented by Whittaker in 
  1923 for life tables, based on penalized least squares. These ideas 
  were revived by Paul Eilers, Leiden University, in 2003. This
  approach is also known as Whittaker-Henderson smoothing.

  The smoother attempts to both fit a curve that represents the raw 
  data, but is penalized if subsequent points vary too much.
  Mathematically it is a large, but sparse optimization problem that 
  can be expressed in a few lines of Matlab or R code.
}
\value{
  A smoothed time series.
}
\note{
  This is a version that avoids package 'SparseM'.
}
\author{
  An R version, based on Matlab code by P. Eilers in 2002, has been 
  published by Nicholas Lewin-Koh on the R-help mailing list in Feb. 
  2004, and in private communication to the author of this package.
}
\references{
  P. H. C. Eilers (2003). A Perfect Smoother. Analytical Chemistry,
  Vol. 75, No. 14, pp. 3631--3636.

  Wilson, D. I. (2006). The Black Art of Smoothing. Electrical and 
  Automation Technology, June/July issue.
}
\seealso{
  \code{\link{supsmu}}, \code{\link{savgol}}, \code{ptw::whit2}
}
\examples{
# **Sinosoid test function**
ts <- sin(2*pi*(1:1000)/200)
t1 <- ts + rnorm(1000)/10
t3 <- whittaker(t1, lambda = 1600)
\dontrun{
plot(1:1000, t1, col = "grey")
lines(1:1000, ts, col="blue")
lines(1:1000, t3, col="red")}
}
\keyword{ timeseries }
