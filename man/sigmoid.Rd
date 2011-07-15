\name{sigmoid}
\alias{sigmoid}
\title{
  Sigmoid Function
}
\description{
  Sigmoid function (aka sigmoidal curve or logistic function).
}
\usage{
sigmoid(x, a = 1, b = 0)
}
\arguments{
  \item{x}{numeric or complex vector.}
  \item{a, b}{parameters.}
}
\details{
  The \code{sigmoidal} function with parameters \code{a,b} is the function
  \deqn{y = \frac{1}{1 + e^{-a (x-b)}}}

  The sigmoid function is also the solution of the ordinary differential equation
  \deqn{y' = y (1-y)}
  with \eqn{y(0) = 1/2} and has an indefinite integral \eqn{\ln(1 + e^x)}.
}
\value{
  Numeric/complex scalar or vector.
}
\examples{
x <- seq(-6, 6, length.out = 101)
y1 <- sigmoid(x)
y2 <- sigmoid(x, a = 2)
\dontrun{
plot(x, y1, type = "l", col = "darkblue", 
        xlab = "", ylab = "", main = "Sigmoid Function(s)")
lines(x, y2, col = "darkgreen")
grid()}

# The slope in 0 (in x = b) is a/4
# sigmf with slope 1 and range [-1, 1].
sigmf <- function(x) 2 * sigmoid(x, a = 2) - 1
}
