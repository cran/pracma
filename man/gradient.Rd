\name{gradient}
\alias{gradient}
\title{
  Discrete Gradient (Matlab Style)
}
\description{
  Discrete numerical gradient.
}
\usage{
gradient(F, h1 = 1, h2 = 1)
}
\arguments{
  \item{F}{vector of function values, or a matrix of values of a function
           of two variables.}
  \item{h1}{x-coordinates of grid points, or one value for the difference
            between grid points in x-direction.}
  \item{h2}{y-coordinates of grid points, or one value for the difference
            between grid points in y-direction.}
}
\details{
  Returns the numerical gradient of a vector or matrix as a vector or matrix
  of discrete slopes in x- (i.e., the differences in horizontal direction)
  and slopes in y-direction (the differences in vertical direction).

  A single spacing value, \code{h}, specifies the spacing between points in
  every direction, where the points are assumed equally spaced.
}
\value{
  If \code{F} is a vector, one gradient vector will be returned.

  If \code{F} is a matrix, a list with two components will be returned:
  \item{X}{numerical gradient/slope in x-direction.}
  \item{Y}{numerical gradient/slope in x-direction.}
  where each matrix is of the same size as \code{F}.
}
\note{
  TODO: If \code{h2} is missing, it will not automatically be adapted.
}
\seealso{
  \code{\link{fderiv}}
}
\examples{
x <- seq(0, 1, by=0.2)
y <- c(1, 2, 3)
(M <- meshgrid(x, y))
gradient(M$X^2 + M$Y^2)
gradient(M$X^2 + M$Y^2, x, y)

\dontrun{
# One-dimensional example
x <- seq(0, 2*pi, length.out = 100)
y <- sin(x)
f <- gradient(y, x)
max(f - cos(x))      #=> 0.00067086
plot(x, y, type = "l", col = "blue")
lines(x, cos(x), col = "gray", lwd = 3)
lines(x, f, col = "red")
grid()

# Two-dimensional example
v <- seq(-2, 2, by=0.2)
X <- meshgrid(v, v)$X
Y <- meshgrid(v, v)$Y

Z <- X * exp(-X^2 - Y^2)
image(v, v, t(Z))
contour(v, v, t(Z), col="black", add = TRUE)
grid(col="white")

grX <- gradient(Z, v, v)$X
grY <- gradient(Z, v, v)$Y

quiver(X, Y, grX, grY, scale = 0.2, col="blue")
}
}
\keyword{ math }
