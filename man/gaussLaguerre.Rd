\name{gaussLaguerre}
\alias{gaussLaguerre}
\title{
  Gauss-Laguerre Quadrature Formula
}
\description{
  Nodes and weights for the n-point Gauss-Laguerre quadrature formula.
}
\usage{
gaussLaguerre(n, a = 0)
}
\arguments{
  \item{n}{Number of nodes in the interval \code{[0, Inf[}.}
  \item{a}{exponent of \code{x} in the integrand: must be greater or equal
           to 0, otherwise the integral would not converge.}
}
\details{
  Gauss-Laguerre quadrature is used for integrating functions of the form
  \deqn{\int_0^{\infty} f(x) x^a e^{-x} dx}
  over the infinite interval \eqn{]0, \infty[}.

  \code{x} and \code{w} are obtained from a tridiagonal eigenvalue problem.
  The value of such an integral is then \code{sum(w*f(x))}.
}
\value{
  List with components \code{x}, the nodes or points in\code{[0, Inf[}, and
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
\code{\link{gaussLegendre}}, \code{\link{gaussHermite}}
}
\examples{
cc <- gaussLaguerre(7)
# integrate exp(-x) from 0 to Inf
sum(cc$w)                     # 1
# integrate x^2 * exp(-x)     # integral x^n * exp(-x) is n!
sum(cc$w * cc$x^2)            # 2
# integrate sin(x) * exp(-x)
cc <- gaussLaguerre(17, 0)    # we need more nodes
sum(cc$w * sin(cc$x))         #=> 0.499999999994907 , should be 0.5
}
\keyword{ math }
