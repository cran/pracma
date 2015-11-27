\name{peaks}
\alias{peaks}
\title{
  Peaks Function (Matlab Style)
}
\description{
  An example functions in two variables, with peaks.
}
\usage{
peaks(v = 49, w)
}
\arguments{
  \item{v}{vector, whose length will be used, or a natural number.}
  \item{w}{another vector, will be used in \code{meshgrid(x,y)}.}
}
\details{
  \code{peaks} is a function of two variables, obtained by translating
  and scaling Gaussian distributions, which is useful for demonstrating
  three-dimensional plots.
}
\value{
  Returns three matrices as a list with \code{X}, \code{Y}, and \code{Z}
  components, the first two being the result of the \code{meshgrid} function,
  and \code{Z} the application of the following function at the points of
  \code{X} and \code{Y}:

  \code{z <-  3 * (1-x)^2 * exp(-(x^2) - (y+1)^2) -
             10 * (x/5 - x^3 - y^5) * exp(-x^2 - y^2) -
            1/3 * exp(-(x+1)^2 - y^2)}
}
\note{
  The variant that \code{peaks()} will display the 3-dim. graph as in Matlab
  is not yet implemented.
}
\seealso{
\code{\link{meshgrid}}
}
\examples{
peaks(3)
\dontrun{
P <- peaks()
x <- P$X[1,]; y <- P$Y[, 1]
persp(x, y, P$Z)
}
}
\keyword{ graphs }
