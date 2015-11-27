\name{linspace}
\alias{linspace}
\title{Linearly Spaced Sequences}
\description{
  Generate linearly spaced sequences.
}
\usage{
  linspace(x1, x2, n = 100)
}
\arguments{
  \item{x1}{numeric scalar specifying starting point}
  \item{x2}{numeric scalar specifying ending point}
  \item{n}{numeric scalar specifying number of points to be generated}
}
\details{
  These functions will generate \code{n} linearly spaced points between
  \code{x1} and \code{x2}.

  If \eqn{n < 2}, the result will be the ending point \code{x2}.
}
\value{
  vector containing \code{n} points between \code{x1} and \code{x2} inclusive.
}
\seealso{
  \code{\link{logspace}}, \code{\link{seq}}
}
\examples{
linspace(1, 10, 9)
}
\keyword{ manip }

