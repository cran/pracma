\name{softline}
\alias{softline}
\title{
  Soft (Inexact) Line Search
}
\description{
  Fletcher's inexact line search algorithm.
}
\usage{
softline(x0, d0, f, g = NULL)
}
\arguments{
  \item{x0}{initial point for linesearch.}
  \item{d0}{search direction from \code{x0}.}
  \item{f}{real function of several variables that is to be minimized.}
  \item{g}{gradient of objective function \code{f}; computed numerically
           if not provided.}
}
\details{
  Many optimization methods have been found to be quite tolerant to line
  search imprecision, therefore inexact line searches are often used in
  these methods.
}
\value{
  Returns the suggested inexact optimization paramater as a real number
  \code{a0} such that \code{x0+a0*d0} should be a reasonable approximation.
}
\note{
  Matlab version of an inexact linesearch algorithm by A. Antoniou and
  W.-S. Lu in their textbook ``Practical Optimization''. Translated to R
  by Hans W Borchers.
}
\references{
  Fletcher, R. (1980). Practical Methods of Optimization, Volume 1.,
  Section 2.6. Wiley, New York.

  Antoniou, A., and W.-S. Lu (2007). Practical Optimization: Algorithms and
  Engineering Applications. Springer Science+Business Media, New York.
}
\seealso{
  \code{\link{gaussNewton}}
}
\examples{
##  Himmelblau function
  f_himm <- function(x) (x[1]^2 + x[2] - 11)^2 + (x[1] + x[2]^2 - 7)^2
  g_himm <- function(x) {
    w1 <- (x[1]^2 + x[2] - 11); w2 <- (x[1] + x[2]^2 - 7)
    g1 <- 4*w1*x[1] + 2*w2;     g2 <- 2*w1 + 4*w2*x[2]
    c(g1, g2)
  }
  # Find inexact minimum from [6, 6] in the direction [-1, -1] !
  softline(c(6, 6), c(-1, -1), f_himm, g_himm)
  # [1] 3.458463

  # Find the same minimum by using the numerical gradient
  softline(c(6, 6), c(-1, -1), f_himm)
  # [1] 3.458463
}
\keyword{ optimize }
