\name{sigmoid}
\alias{sigmoid}
\alias{logit}
\title{
  Sigmoid Function
}
\description{
  Sigmoid function (aka sigmoidal curve or logistic function).
}
\usage{
sigmoid(x, a = 1, b = 0)
logit(x, a = 1, b = 0)
}
\arguments{
  \item{x}{numeric vector.}
  \item{a, b}{parameters.}
}
\details{
  The \code{sigmoidal} function with parameters \code{a,b} is the function
  \deqn{y = 1/(1 + e^{-a (x-b)})}

  The \code{sigmoid} function is also the solution of the ordinary 
  differentialequation
  \deqn{y' = y (1-y)}
  with \eqn{y(0) = 1/2} and has an indefinite integral \eqn{\ln(1 + e^x)}.

  The \code{logit} function is the inverse of the sigmoid function and is
  (therefore) omly defined between 0 and 1. Its definition is
  \deqn{y = b + 1/a log(x/(1-x))}

  The parameters must be scalars; if they are vectors, only the first
  component will be taken.
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

# logit is the inverse of the sigmoid function
x <- c(-0.75, -0.25, 0.25, 0.75)
y <- sigmoid(x)
logit(y)        #=> -0.75 -0.25  0.25  0.75
}
