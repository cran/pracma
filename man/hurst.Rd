\name{hurstexp}
\alias{hurstexp}
\title{
  Hurst Exponent
}
\description{
  Calculates the Hurst exponent using R/S analysis.
}
\usage{
  hurstexp(x, d = 50, display = TRUE)
}
\arguments{
  \item{x}{a time series.}
  \item{d}{smallest box size; default 50.}
  \item{display}{logical; shall the results be printed to the console?}
}
\details{
  \code{hurstexp(x)} calculates the Hurst exponent of a time series \code{x}
  using R/S analysis, after Hurst, with slightly different approaches, or
  corrects it with small sample bias, see for example Weron.

  These approaches are a corrected R/S method, an empirical and corrected
  empirical method, and a try at a theoretical Hurst exponent. It should be
  mentioned that the results are sometimes very different, so providing error
  estimates will be highly questionable.

  Optimal sample sizes are automatically computed with a length that
  possesses the most divisors among series shorter than \code{x} by no more
  than 1 percent.
}
\value{
  \code{hurstexp(x)} returns a list with the following components:
  \itemize{
    \item \code{Hs}  - simplified R over S approach
    \item \code{Hrs} - corrected R over S Hurst exponent
    \item \code{He}  - empirical Hurst exponent
    \item \code{Hal} - corrected empirical Hurst exponent
    \item \code{Ht}  - theoretical Hurst exponent
  }
}
\note{
  Derived from Matlab code of R. Weron, published on Matlab Central.
}
\references{
  H.E. Hurst (1951) Long-term storage capacity of reservoirs, Transactions
  of the American Society of Civil Engineers 116, 770-808.

  R. Weron (2002) Estimating long range dependence: finite sample properties
  and confidence intervals, Physica A 312, 285-299.
}
\seealso{
  \code{fractal::hurstSpec, RoverS, hurstBlock} and \code{fArma::LrdModelling}
}
\examples{
##  Computing the Hurst exponent
data(brown72)
x72 <- brown72                          #  H = 0.72
xgn <- rnorm(1024)                      #  H = 0.50
xlm <- numeric(1024); xlm[1] <- 0.1     #  H = 0.43
for (i in 2:1024) xlm[i] <- 4 * xlm[i-1] * (1 - xlm[i-1])

hurstexp(brown72, d = 128)           # 0.72
# Simple R/S Hurst estimation:         0.6590931 
# Corrected R over S Hurst exponent:   0.7384611 
# Empirical Hurst exponent:            0.7068613 
# Corrected empirical Hurst exponent:  0.6838251 
# Theoretical Hurst exponent:          0.5294909

hurstexp(xgn)                        # 0.50
# Simple R/S Hurst estimation:         0.5518143 
# Corrected R over S Hurst exponent:   0.5982146 
# Empirical Hurst exponent:            0.6104621 
# Corrected empirical Hurst exponent:  0.5690305 
# Theoretical Hurst exponent:          0.5368124 

hurstexp(xlm)                        # 0.43
# Simple R/S Hurst estimation:         0.4825898 
# Corrected R over S Hurst exponent:   0.5067766 
# Empirical Hurst exponent:            0.4869625 
# Corrected empirical Hurst exponent:  0.4485892 
# Theoretical Hurst exponent:          0.5368124 


##  Compare with other implementations
\dontrun{
library(fractal)

x <- x72
hurstSpec(x)                    # 0.776   # 0.720
RoverS(x)                       # 0.717
hurstBlock(x, method="aggAbs")  # 0.648
hurstBlock(x, method="aggVar")  # 0.613
hurstBlock(x, method="diffvar") # 0.714
hurstBlock(x, method="higuchi") # 1.001

x <- xgn
hurstSpec(x)                    # 0.538   # 0.500
RoverS(x)                       # 0.663
hurstBlock(x, method="aggAbs")  # 0.463
hurstBlock(x, method="aggVar")  # 0.430
hurstBlock(x, method="diffvar") # 0.471
hurstBlock(x, method="higuchi") # 0.574

x <- xlm
hurstSpec(x)                    # 0.478   # 0.430
RoverS(x)                       # 0.622
hurstBlock(x, method="aggAbs")  # 0.316
hurstBlock(x, method="aggVar")  # 0.279
hurstBlock(x, method="diffvar") # 0.547
hurstBlock(x, method="higuchi") # 0.998
}
}
\keyword{ timeseries }
