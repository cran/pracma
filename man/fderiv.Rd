\name{fderiv}
\alias{fderiv}
\title{
  Numerical Differentiation
}
\description{
  Numerical function differentiation for orders \code{n=1..4} using
  finite difference approximations.
}
\usage{
fderiv(f, x, n = 1, h = 0,
        method = c("central", "forward", "backward"), ...)
}
\arguments{
  \item{f}{function to be differentiated.}
  \item{x}{point(s) where differentiation will take place.}
  \item{n}{order of derivative, should only be between 1 and 8;
           for \code{n=0} function values will be returned.}
  \item{h}{step size: if \code{h=0} step size will be set automatically.}
  \item{method}{one of ``central'', ``forward'', or ``backward''.}
  \item{...}{more variables to be passed to function \code{f}.}
}
\details{
  Derivatives are computed applying central difference formulas that stem
  from the Taylor series approximation. These formulas have a convergence
  rate of \eqn{O(h^2)}.

  Use the `forward' (right side) or `backward' (left side) method if the
  function can only be computed or is only defined on one side. Otherwise,
  always use the central difference formulas.

  Optimal step sizes depend on the accuracy the function can be computed with.
  Assuming internal functions with an accuracy 2.2e-16, appropriate step
  sizes might be \code{5e-6, 1e-4, 5e-4, 2.5e-3} for \code{n=1,...,4} and
  precisions of about \code{10^-10, 10^-8, 5*10^-7, 5*10^-6} (at best).

  For \code{n>4} a recursion (or finite difference) formula will be applied,
  cd. the Wikipedia article on ``finite difference''.
}
\value{
  Vector of the same length as \code{x}.
}
\references{
  Kiusalaas, J. (2005). Numerical Methods in Engineering with Matlab.
  Cambridge University Press.
}
\note{
  Numerical differentiation suffers from the conflict between round-off
  and truncation errors. 
}
\seealso{
  \code{\link{numderiv}}, \code{\link{taylor}}
}
\examples{
\dontrun{
f <- sin
xs <- seq(-pi, pi, length.out = 100)
ys <- f(xs)
y1 <- fderiv(f, xs, n = 1, method = "backward")
y2 <- fderiv(f, xs, n = 2, method = "backward")
y3 <- fderiv(f, xs, n = 3, method = "backward")
y4 <- fderiv(f, xs, n = 4, method = "backward")
plot(xs, ys, type = "l", col = "gray", lwd = 2,
     xlab = "", ylab = "", main = "Sinus and its Derivatives")
lines(xs, y1, col=1, lty=2)
lines(xs, y2, col=2, lty=3)
lines(xs, y3, col=3, lty=4)
lines(xs, y4, col=4, lty=5)
grid()}
}
\keyword{ math }
