\name{jacobian}
\alias{jacobian}
\title{Jacobian Matrix}
\description{
  Jacobian matrix of a function R^n --> R^m .
}
\usage{
jacobian(f, x0, heps = .Machine$double.eps^(1/3), ...)
}
\arguments{
\item{f}{\code{m} functions of \code{n} variables.}
\item{x0}{Numeric vector of length \code{n}.}
\item{heps}{This is \code{h} in the derivative formula.}
\item{...}{parameters to be passed to f.}
}
\details{
  Computes the derivative of each funktion \eqn{f_j} by variable \eqn{x_i}
  separately, taking the discrete step \eqn{h}.
}
\value{
  Numeric \code{m}-by-\code{n} matrix \code{J} where the entry \code{J[j, i]}
  is \eqn{\frac{\partial f_j}{\partial x_i}}, i.e. the derivatives of function
  \eqn{f_j} line up in row \eqn{i} for \eqn{x_1, \ldots, x_n}.
}
\references{
  Quarteroni, A., R. Sacco, and F. Saleri (2007). Numerical Mathematics.
  Second Edition, Springer-Verlag, Berlin Heidelberg.
}
\note{
  Obviously, this function is \emph{not} vectorized.
}
\seealso{
\code{gradient}
}
\examples{
##  Example function from Quarteroni & Saleri
f <- function(x) c(x[1]^2 + x[2]^2 - 1, sin(pi*x[1]/2) + x[2]^3)
jf <- function(x) 
          matrix( c(2*x[1], pi/2 * cos(pi*x[1]/2), 2*x[2], 3*x[2]^2), 2, 2)
all.equal(jf(c(1,1)), jacobian(f, c(1,1)))
# TRUE
}
\keyword{ math }
