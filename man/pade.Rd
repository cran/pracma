\name{pade}
\alias{pade}
\title{
  Pade Approximation
}
\description{
  A Pade approximation is a rational function (of a specified order) whose
  power series expansion agrees with a given function and its derivatives
  to the highest possible order.
}
\usage{
pade(p1, p2 = c(1), d1 = 5, d2 = 5)
}
\arguments{
  \item{p1}{polynomial representing or approximating the function,
            preferably the Taylor series of the function around some point.}
  \item{p2}{if present, the function is given as \code{p1/p2}.}
  \item{d1}{the degree of the numerator of the rational function.}
  \item{d2}{the degree of the denominator of the rational function.}
}
\details{
  The relationship between the coefficients of \code{p1} (and \code{p2})
  and \code{r1} and \code{r2} is determined by a system of linear equations.
  The system is then solved by applying the pseudo-inverse \code{pinv} for
  for the left-hand matrix.
}
\value{
  List with components \code{r1} and \code{r2} for the numerator and 
  denominator polynomials, i.e. \code{r1/r2} is the rational approximation
  sought.
}
\note{
  In general, errors for Pade approximations are smallest when the degrees
  of numerator and denominator are the same or when the degree of the
  numerator is one larger than that of the denominator.
}
\references{
  Press, W. H., S. A. Teukolsky, W. T Vetterling, and B. P. Flannery (2007).
  Numerical Recipes: The Art of Numerical Computing. Third Edition,
  Cambridge University Press, New York.
}
\seealso{
  \code{\link{taylor}}, \code{ratInterp}
}
\examples{
##  Exponential function
p1 <- c(1/24, 1/6, 1/2, 1.0, 1.0)  # Taylor series of exp(x) at x=0
R  <- pade(p1); r1 <- R$r1; r2 <- R$r2
f1 <- function(x) polyval(r1, x) / polyval(r2, x)
\dontrun{
xs <- seq(-1, 1, length.out=51); ys1 <- exp(xs); ys2 <- f1(xs)
plot(xs, ys1, type = "l", col="blue")
lines(xs, ys2, col = "red")
grid()}
}
\keyword{ math }
