\name{shubert}
\alias{shubert}
\title{
  Shubert-Piyavskii Method
}
\description{
  Shubert-Piyavskii Univariate Function Maximization
}
\usage{
shubert(f, a, b, L, crit = 1e-04, nmax = 1000)
}
\arguments{
  \item{f}{function to be optimized.}
  \item{a, b}{search between a and b for a maximum.}
  \item{L}{a Lipschitz constant for the function.}
  \item{crit}{critical value}
  \item{nmax}{maximum number of steps.}
}
\details{
  The Shubert-Piyavskii method, often called the Sawtooth Method, finds 
  the global maximum of a univariate function on a known interval. It is 
  guaranteed to find the global maximum on the interval under certain
  conditions:

  The function f is Lipschitz-continuous, that is there is a constant L
  such that \deqn{|f(x) - f(y)| \le L |x - y|}
  for all \eqn{x, y} in \eqn{[a, b]}.

  The process is stopped when the improvement in the last step is smaller 
  than the input argument \code{crit}.
}
\value{
Returns a list with the following components:
\item{xopt}{the x-coordinate of the minimum found.}
\item{fopt}{the function value at the minimum.}
\item{nopt}{number of steps.}
}
\references{
  Y. K. Yeo. Chemical Engineering Computation with MATLAB.
  CRC Press, 2017.
}
\seealso{
  \code{\link{findmins}}
}
\examples{
# Determine the global minimum of sin(1.2*x)+sin(3.5*x) in [-3, 8].
f <- function(x) sin(1.2*x) + sin(3.5*x)
shubert(function(x) -f(x), -3, 8, 5, 1e-04, 1000)
## $xopt
## [1] 3.216231     # 3.216209
## $fopt
## [1] 1.623964
## $nopt
## [1] 481
}
\keyword{ optimize }
