\name{lebesgue}
\alias{lebesgue}
\title{Lebesgue Constant}
\description{
  Estimates the Lebesgue constant.
}
\usage{
lebesgue(x, refine = 4, plotting = FALSE)
}
\arguments{
    \item{x}{numeric vector of grid points}
    \item{refine}{refine the grid with \code{2^refine} grid points;
                  can only be an integer between 2 and 10, default 4.}
    \item{plotting}{shall the Lebesgue function be plotted.}
}
\details{
  The Lebesgue constant gives an estimation \eqn{||P_n f|| \le L ||f||}
  (in minimax norm) where \eqn{P_n f} is the interpolating polynomial of
  order \eqn{n} for \eqn{f} on an interval \eqn{[a, b]}.
}
\value{
  Lebesgue constant for the given grid points.
}
\references{
  Berrut, J.-P., and L. Nick Trefethen (2004). ``Barycentric Lagrange
  Interpolation''. SIAM Review, Vol. 46(3), pp.501--517.
}
\note{
  The Lebesgue constant plays an important role when estimating the distance
  of interpolating polynomials from the minimax solution (see the Remez
  algorithm).
}
\seealso{
\code{\link{barylag}}
}
\examples{
lebesgue(seq(0, 1, length.out = 6))  #=> 3.100425
}
\keyword{ math }
