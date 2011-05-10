\name{runge}
\alias{runge}
\title{Runge Function}
\description{
Runge's test function for interpolation techniques.
}
\usage{
runge(x)
}
\arguments{
  \item{x}{numeric scalar.}
}
\details{
  Runge's function is a classical test function for interpolation and 
  and approximation techniques, especially for equidistant nodes.

  For example, when approximating the Runge function on the interval
  \code{[-1, 1]}, the error at the endpoints will diverge when the number
  of nodes is increasing.
}
\value{
Numerical value of the function.
}
\seealso{
  \code{\link{fnorm}}
}
\examples{
\dontrun{
x <- seq(-1, 1, length.out = 101)
y <- runge(x)
plot(x, y, type = "l", lwd = 2, col = "navy", ylim = c(-0.2, 1.2))
grid()

n <- c(6, 11, 16)
for (i in seq(along=n)) {
    xp <- seq(-1, 1, length.out = n[i])
    yp <- runge(xp)
    p  <- polyfit(xp, yp, n[i]-1)
    y  <- polyval(p, x)
    lines(x, y, lty=i) }
}
}
\keyword{ math }
