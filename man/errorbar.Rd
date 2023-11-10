\name{errorbar}
\alias{errorbar}
\title{
  Plot Error Bars
}
\description{
  Draws symmetric error bars in x- and/or y-direction.
}
\usage{
errorbar(x, y, xerr = NULL, yerr = NULL,
         bar.col = "red", bar.len = 0.01,
         grid = TRUE, with = TRUE, add = FALSE, ...)
}
\arguments{
  \item{x, y}{x-, y-coordinates}
  \item{xerr, yerr}{length of the error bars, relative to the x-, y-values.}
  \item{bar.col}{color of the error bars; default: red}
  \item{bar.len}{length of the cross bars orthogonal to the error bars;
                 default: 0.01.}
  \item{grid}{logical; should the grid be plotted?; default: true}
  \item{with}{logical; whether to end the error bars with small cross bars.}
  \item{add}{logical; should the error bars be added to an existing plot?;
             default: false.}
  \item{...}{additional plotting parameters that will be passed to the
             \code{plot} function.}
}
\details{
  \code{errorbar} plots \code{y} versus \code{x} with symmetric error bars,
  with a length determined by \code{xerr} resp. \code{yerr} in x- and/or
  y-direction. If \code{xerr} or \code{yerr} is \code{NULL} error bars in
  this direction will not be drawn.

  A future version will allow to draw unsymmetric error bars by specifying
  upper and lower limits when \code{xerr} or \code{yerr} is a matrix of
  size \code{(2 x length(x))}.
}
\value{
  Generates a plot, no return value.
}
\seealso{
  \code{plotrix::plotCI}, \code{Hmisc::errbar}
}
\examples{
\dontrun{
x <- seq(0, 2*pi, length.out = 20)
y <- sin(x)
xe <- 0.1
ye <- 0.1 * y
errorbar(x, y, xe, ye, type = "l", with = FALSE)

cnt <- round(100*randn(20, 3))
y <- apply(cnt, 1, mean)
e <- apply(cnt, 1, sd)
errorbar(1:20, y, yerr = e, bar.col = "blue")
}
}
\keyword{ graphs }
