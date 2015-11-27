\name{triquad}
\alias{triquad}
\title{
  Gaussian Triangle Quadrature
}
\description{
  Numerically integrates a function over an arbitrary triangular domain by
  computing the Gauss nodes and weights.
}
\usage{
triquad(f, x, y, n = 10, tol = 1e-10, ...)
}
\arguments{
  \item{f}{the integrand as function of two variables.}
  \item{x}{x-coordinates of the three vertices of the triangle.}
  \item{y}{y-coordinates of the three vertices of the triangle.}
  \item{n}{number of nodes.}
  \item{tol}{relative tolerance to be achieved.}
  \item{\ldots}{additional parameters to be passed to the function.}
}
\details{
  Computes the \code{N^2} nodes and weights for a triangle with vertices
  given by 3x2 vector. The nodes are produced by collapsing the square
  to a triangle.

  Then \code{f} will be applied to the nodes and the result multiplied
  left and right with the weights (i.e., Gaussian quadrature).

  By default, the function applies Gaussian quadrature with number of
  nodes \code{n=10,21,43,87,175} until the relative error is smaller than
  the tolerance.
}
\value{
  The integral as a scalar.
}
\note{
  A small relative tolerance is \emph{not} really indicating a small
  absolute tolerance.
}
\author{
  Copyright (c) 2005 Greg von Winckel Matlab code based on the publication
  mentioned and available from MatlabCentral (calculates nodes and weights).
  Translated to R (with permission) by Hans W Borchers.
}
\references{
  Lyness, J. N., and R. Cools (1994). A Survey of Numerical Cubature
  over Triangles. Proceedings of the AMS Conference ``Mathematics of
  Computation 1943--1993'', Vancouver, CA.
}
\seealso{
  \code{\link{quad2d}}, \code{\link{simpson2d}}
}
\examples{
x <- c(-1, 1, 0); y <- c(0, 0, 1)
f1 <- function(x, y) x^2 + y^2
(I <- triquad(f1, x, y))                        # 0.3333333333333333

# split the unit square
x1 <- c(0, 1, 1); y1 <- c(0, 0, 1)
x2 <- c(0, 1, 0); y2 <- c(0, 1, 1)
f2 <- function(x, y) exp(x + y)
I <- triquad(f2, x1, y1) + triquad(f2, x2, y2)  # 2.952492442012557
quad2d(f2, 0, 1, 0, 1)                          # 2.952492442012561
simpson2d(f2, 0, 1, 0, 1)                       # 2.952492442134769
dblquad(f2,  0, 1, 0, 1)                        # 2.95249244201256
}
\keyword{ math }
