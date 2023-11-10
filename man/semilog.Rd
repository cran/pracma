\name{semilogx,semilogy}
\alias{semilogx}
\alias{semilogy}
\alias{loglog}
\title{
  Semi-logarithmic Plots (Matlab Style)
}
\description{
  Generates semi- and double-logarithmic plots.
}
\usage{
semilogx(x, y, ...)
semilogy(x, y, ...)

loglog(x, y, ...)
}
\arguments{
  \item{x, y}{x-, y-coordinates.}
  \item{...}{additional graphical parameters passed to the plot function.}
}
\details{
  Plots data in logarithmic scales for the x-axis or y-axis, or uses
  logarithmic scales in both axes, and adds grid lines.
}
\value{
  Generates a plot, returns nothing.
}
\note{
  Matlab's logarithmic plots find a more appropriate grid.
}
\seealso{
  \code{\link{plot}} with \code{log= } option.
}
\examples{
\dontrun{
x <- logspace(-1, 2)
loglog(x, exp(x), type = 'b')}
}
\keyword{ graphs }
