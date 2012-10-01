\name{Testfunctions}
\alias{rastrigin}
\alias{rosenbrock}
\title{
  Optimization Test Function
}
\description{
  Simple and often used test function defined in higher dimensions.
  The dimension is determined by the length of the input vector.
}
\usage{
rastrigin(x)
rosenbrock(x)
}
\arguments{
  \item{x}{numeric vector of some length \code{n >= 2}.}
}
\details{
\strong{Rastrigin}:
  \deqn{10 n + \sum_1^n (x_i^2 - 10 \cos(2 \pi x_i))}
\tabular{ll}{
  No. of Vars.: \tab n >= 1 \cr
  Bounds: \tab -5.12 <= xi <= 5.12 \cr
  Local minima: \tab many \cr
  Minimum: \tab 0.0 \cr
  Solution: \tab xi = 0, i = 1:n \cr
}
  \strong{Rosenbrock}:
    \deqn{\sum_{i=1}^{n-1} (1-x_i)^2 + 100 (x_{i+1}-x_i^2)^2}
  \tabular{ll}{
    No. of Vars.: \tab n >= 2 \cr
    Bounds: \tab -5.12 <= xi <= 5.12 \cr
    Local minima: \tab at f(-1, 1, \ldots, 1) for n >= 4 \cr
    Minimum: \tab 0.0 \cr
    Solution: \tab xi = 1, i = 1:n \cr
  }
}
\value{
  Returns the value of the function at the specified point.
}
\references{
  See the Wikipedia.
}
\examples{
rastrigin(c(1, 1, 1, 1))
rosenbrock(c(1, 1, 1, 1))
}
