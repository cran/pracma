\name{vectorfield}
\alias{vectorfield}
\title{
  Vector Field Plotting
}
\description{
  Plotting a vector field
}
\usage{
vectorfield(fun, xlim, ylim, n = 16,
            scale = 0.05, col = "green", ...)
}
\arguments{
  \item{fun}{function of two variables --- must be vectorized.}
  \item{xlim}{range of \code{x} values.}
  \item{ylim}{range of \code{y} values.}
  \item{n}{grid size, proposed 16 in each direction.}
  \item{scale}{scales the length of the arrows.}
  \item{col}{arrow color, proposed `green'.}
  \item{...}{more options presented to the \code{arrows} primitive.}
}
\details{
  Plots a vector field for a function \code{f}. Main usage could be to plot
  the solution of a differential equation into the same graph.
}
\value{
  Opens a graph window and plots the vector field.
}
\seealso{
  \code{\link{quiver}}, \code{\link{arrows}}
}
\examples{
f <- function(x, y) x^2 - y^2
xx <- c(-1, 1); yy <- c(-1, 1)
\dontrun{
vectorfield(f, xx, yy, scale = 0.1)
for (xs in seq(-1, 1, by = 0.25)) {
    sol <- rk4(f, -1, 1, xs, 100)
    lines(sol$x, sol$y, col="darkgreen")
}
grid()}
}
\keyword{ graphs }
