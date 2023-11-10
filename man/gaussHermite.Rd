\name{gaussHermite}
\alias{gaussHermite}
\title{
  Gauss-Hermite Quadrature Formula
}
\description{
  Nodes and weights for the n-point Gauss-Hermite quadrature formula.
}
\usage{
gaussHermite(n)
}
\arguments{
  \item{n}{Number of nodes in the interval \code{]-Inf, Inf[}.}
}
\details{
  Gauss-Hermite quadrature is used for integrating functions of the form
  \deqn{\int_{-\infty}^{\infty} f(x) e^{-x^2} dx}
  over the infinite interval \eqn{]-\infty, \infty[}.

  \code{x} and \code{w} are obtained from a tridiagonal eigenvalue problem.
  The value of such an integral is then \code{sum(w*f(x))}.
}
\value{
  List with components \code{x}, the nodes or points in\code{]-Inf, Inf[}, and
  \code{w}, the weights applied at these nodes.
}
\references{
  Gautschi, W. (2004). Orthogonal Polynomials: Computation and Approximation.
  Oxford University Press.

  Trefethen, L. N. (2000). Spectral Methods in Matlab. SIAM, Society for
  Industrial and Applied Mathematics.
}
\note{
  The basic quadrature rules are well known and can, e. g., be found in
  Gautschi (2004) --- and explicit Matlab realizations in Trefethen (2000).
  These procedures have also been implemented in Matlab by Geert Van Damme,
  see his entries at MatlabCentral since 2010.
}
\seealso{
\code{\link{gaussLegendre}}, \code{\link{gaussLaguerre}}
}
\examples{
cc <- gaussHermite(17)
# Integrate  exp(-x^2)  from -Inf to Inf
sum(cc$w)                        #=> 1.77245385090552 == sqrt(pi)
# Integrate  x^2 exp(-x^2)
sum(cc$w * cc$x^2)               #=> 0.88622692545276 == sqrt(pi) /2
# Integrate  cos(x) * exp(-x^2)
sum(cc$w * cos(cc$x))            #=> 1.38038844704314 == sqrt(pi)/exp(1)^0.25
}
\keyword{ math }
