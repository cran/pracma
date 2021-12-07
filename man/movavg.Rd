\name{movavg}
\alias{movavg}
\title{
  Moving Average Filters
}
\description{
  Different types of moving average of a time series.
}
\usage{
movavg(x, n, type=c("s", "t", "w", "m", "e", "r"))
}
\arguments{
  \item{x}{time series as numeric vector.}
  \item{n}{backward window length.}
  \item{type}{one of 's', 't', 'w', 'm', 'e', or 'r'; default is 's'.}
}
\details{
  Types of available moving averages are:
  \itemize{
  \item \code{s} for ``simple'', it computes the simple moving average.
        \code{n} indicates the number of previous data points used with the 
        current data point when calculating the moving average.
  \item \code{t} for ``triangular'', it computes the triangular moving average 
        by calculating the first simple moving average with window width of 
        \code{ceil(n+1)/2}; then it calculates a second simple moving average 
        on the first moving average with the same window size.
  \item \code{w} for ``weighted", it calculates the weighted moving average 
        by supplying weights for each element in the moving window. Here the 
        reduction of weights follows a linear trend.
  \item \code{m} for ``modified", it calculates the modified moving average. 
        The first modified moving average is calculated like a simple moving 
        average. Subsequent values are calculated by adding the new value and 
        subtracting the last average from the resulting sum.
  \item \code{e} for``exponential", it computes the exponentially weighted 
        moving average. The exponential moving average is a weighted moving 
        average that reduces influences by applying more weight to recent 
        data points () reduction factor \code{2/(n+1)}; or
  \item \code{r} for``running", this is an exponential moving average with a 
        reduction factor of \code{1/n} [same as the modified average?].
  }
}
\value{
  Vector the same length as time series \code{x}.
}
\references{
  Matlab Techdoc
}
\seealso{
  \code{filter}
}
\examples{
\dontrun{
abbshares <- scan(file="")
25.69 25.89 25.86 26.08 26.41 26.90 26.27 26.45 26.49 26.08 26.11 25.57 26.02
25.53 25.27 25.95 25.19 24.78 24.96 24.63 25.68 25.24 24.87 24.71 25.01 25.06
25.62 25.95 26.08 26.25 25.91 26.61 26.34 25.55 25.36 26.10 25.63 25.52 24.74
25.00 25.38 25.01 24.57 24.95 24.89 24.13 23.83 23.94 23.74 23.12 23.13 21.05
21.59 19.59 21.88 20.59 21.59 21.86 22.04 21.48 21.37 19.94 19.49 19.46 20.34
20.59 19.96 20.18 20.74 20.83 21.27 21.19 20.27 18.83 19.46 18.90 18.09 17.99
18.03 18.50 19.11 18.94 18.21 18.06 17.66 16.77 16.77 17.10 17.62 17.22 17.95
17.08 16.42 16.71 17.06 17.75 17.65 18.90 18.80 19.54 19.23 19.48 18.98 19.28
18.49 18.49 19.08 19.63 19.40 19.59 20.37 19.95 18.81 18.10 18.32 19.02 18.78
18.68 19.12 17.79 18.10 18.64 18.28 18.61 18.20 17.82 17.76 17.26 17.08 16.70
16.68 17.68 17.70 18.97 18.68 18.63 18.80 18.81 19.03 18.26 18.78 18.33 17.97
17.60 17.72 17.79 17.74 18.37 18.24 18.47 18.75 18.66 18.51 18.71 18.83 19.82
19.71 19.64 19.24 19.60 19.77 19.86 20.23 19.93 20.33 20.98 21.40 21.14 21.38
20.89 21.08 21.30 21.24 20.55 20.83 21.57 21.67 21.91 21.66 21.53 21.63 21.83
21.48 21.71 21.44 21.67 21.10 21.03 20.83 20.76 20.90 20.92 20.80 20.89 20.49
20.70 20.60 20.39 19.45 19.82 20.28 20.24 20.30 20.66 20.66 21.00 20.88 20.99
20.61 20.45 20.09 20.34 20.61 20.29 20.20 20.00 20.41 20.70 20.43 19.98 19.92
19.77 19.23 19.55 19.93 19.35 19.66 20.27 20.10 20.09 20.48 19.86 20.22 19.35
19.08 18.81 18.87 18.26 18.27 17.91 17.68 17.73 17.56 17.20 17.14 16.84 16.47
16.45 16.25 16.07

plot(abbshares, type = "l", col = 1, ylim = c(15, 30),
                main = "Types of moving averages", sub = "Mid 2011--Mid 2012",
                xlab = "Days", ylab = "ABB Shares Price (in USD)")
y <- movavg(abbshares, 50, "s"); lines(y, col = 2)
y <- movavg(abbshares, 50, "t"); lines(y, col = 3)
y <- movavg(abbshares, 50, "w"); lines(y, col = 4)
y <- movavg(abbshares, 50, "m"); lines(y, col = 5)
y <- movavg(abbshares, 50, "e"); lines(y, col = 6)
y <- movavg(abbshares, 50, "r"); lines(y, col = 7)
grid()
legend(120, 29, c("original data", "simple", "triangular", "weighted",
                                   "modified", "exponential", "running"),
                col = 1:7, lty = 1, lwd = 1, box.col = "gray", bg = "white")
}
}
\keyword{ timeseries }
