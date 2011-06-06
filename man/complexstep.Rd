\name{compexstep}
\alias{complexstep}
\alias{jacobiancsd}
\title{Complex Step Derivation}
\description{
  Complex step derivatives of real-valued functions.
}
\usage{
complexstep(f, x0, h = 1e-20, test = FALSE, ...)

jacobiancsd(f, x0, h = 1e-20, ...)
}
\arguments{
  \item{f}{Function that is to be differentiated.}
  \item{x0}{Point at which to differentiate the function.}
  \item{h}{Step size to be applied; shall be \emph{very} small.}
  \item{test}{logical; if \code{TRUE} the complex step derivative will be
              compared to the central derivative formula and a warning
              issued if the difference is too large.}
  \item{...}{Additional variables to be passed to \code{f}.}
}
\details{
  Complex step derivation is a fast and highly exact way of numerically
  differentiating a function. If the following conditions are satisfied,
  there will be no loss of accuracy between computing a function value
  and computing the derivative at a certain point.
  \itemize{
    \item \code{f} must have an analytical (i.e., complex differentiable)
    continuation into an open neighborhood of \code{x0}.
    \item \code{f(x0)} must be real.
    \item \code{h} is real and \emph{very} small: \code{0 < h << 1}.
  }
}
\value{
  \code{complexstep(f, x0)} returns the derivative \eqn{f'(x_0)} of \eqn{f}
  at \eqn{x_0}. The function is vectorized in \code{x0}.  
}
\references{
  Martins, J. R. R. A., P. Sturdza, and J. J. Alonso (2003).
  The Complex-step Derivative Approximation.
  ACM Transactions on Mathematical Software, Vol. 29, No. 3, pp. 245--262.
}
\author{
  HwB <hwborchers@googlemail.com>
}
\note{
  This surprising approach can be easily deduced from the complex-analytic
  Taylor formula.
}
\seealso{
\code{\link{jacobian}}
}
\examples{
##  Example from Martins et al.
f <- function(x) exp(x)/sqrt(sin(x)^3 + cos(x)^3)  # derivative at x0 = 1.5
# central diff formula    # 4.05342789402801, error 1e-10
# numDeriv::grad(f, 1.5)  # 4.05342789388197, error 1e-12  Richardson
# pracma::numderiv        # 4.05342789389868, error 5e-14  Richardson
complexstep(f, 1.5)       # 4.05342789389862, error 1e-15
# Symbolic calculation:   # 4.05342789389862

jacobiancsd(f, 1.5)
}
\keyword{ math }
