\name{nile}
\alias{nile}
\docType{data}
\title{
  Nile overflow data
}
\description{
  Nile overflow data 1871--1984, gathered mostly by H. E. Hurst.
}
\usage{data("nile")}
\format{
  A data frame with 114 years of observations during the months Jan to Dec.
}
\details{
  Monthly flow data taken at the Dongola measurement station just upstream
  from the high dam at Aswan.
}
\references{
  R. Weron (2001). Estimating long range dependence: finite sample properties and
  confidence intervals. arXiv.org:cond-mat0103510.pdf .
}
\examples{
data(nile)                      # loads "nile" data frame
\dontrun{
nile_dt <- nile[, 2:13]         # erase the "years" column

# plot all years in one figure
plot(ts(nile_dt), plot.type="single")

# merge all years in one time series
nile_ts <- ts( c(t(nile[, 2:13])), frequency = 12, start = c(1871, 1) )

# aggregated flow per year
nile_flow <- apply(nile_dt, 1, sum)

plot(ts(nile_flow, frequency = 1, start = 1871) / 1000,
     col = "darkblue", lwd = 2.0,
     main = "Nile flows 1871 - 1984", ylab = "Flow / 1000")
grid()

# Hurst exponent of yearly Nile flow
hurstexp(nile_flow)
# Simple R/S Hurst estimation:         0.7348662 
# Corrected R over S Hurst exponent:   1.041862 
# Empirical Hurst exponent:            0.6975531 
# Corrected empirical Hurst exponent:  0.7136607 
# Theoretical Hurst exponent:          0.5244148 
}
}
\keyword{datasets}
