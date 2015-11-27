\name{ppfit}
\alias{ppfit}
\title{
  Piecewise Polynomial Fit
}
\description{
  Piecewise linear or cubic fitting.
}
\usage{
ppfit(x, y, xi, method = c("linear", "cubic"))
}
\arguments{
  \item{x, y}{x-, y-coordinates of given points.}
  \item{xi}{x-coordinates of the choosen support nodes.}
  \item{method}{interpolation method,
                can be `constant', `linear', or `cubic' (i.e., `spline').}
}
\details{
  \code{ppfit} fits a piece-wise polynomial to the input independent and 
    dependent variables,\code{x} and \code{y}, respectively. A weighted linear 
    least squares solution is provided. The weighting vector \code{w} must be 
    of the same size as the input variables.
}
\value{
  Returns a \code{pp} (i.e., piecewise polynomial) structure.
}
\note{
  Following an idea of Copyright (c) 2012 Ben Abbott, Martin Helm for Octave.
}
\seealso{
  \code{\link{mkpp}}, \code{\link{ppval}}
}
\examples{
x <- 0:39
y <- c(  8.8500,  32.0775,  74.7375, 107.6775, 132.0975, 156.6675,
       169.0650, 187.5375, 202.2575, 198.0750, 225.9600, 204.3550,
       233.8125, 204.5925, 232.3625, 204.7550, 220.1925, 199.5875,
       197.3025, 175.3050, 218.6325, 163.0775, 170.6625, 148.2850,
       154.5950, 135.4050, 138.8600, 125.6750, 118.8450,  99.2675,
       129.1675,  91.1925,  89.7000,  76.8825,  83.6625,  74.1950,
        73.9125,  55.8750,  59.8675,  48.1900)

xi <- linspace(0, 39, 8)
pplin <- ppfit(x, y, xi)  # method = "linear"
ppcub <- ppfit(x, y, xi, method = "cubic")

\dontrun{
plot(x, y, type = "b", main = "Piecewise polynomial approximation")
xs <- linspace(0, 39, 100)
yslin <- ppval(pplin, xs)
yscub <- ppval(ppcub, xs)
lines(xs, yscub, col="red",lwd = 2)
lines(xs, yslin, col="blue")
grid()}
}
\keyword{ fitting }
