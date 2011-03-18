\name{linspace, logspace}
\alias{linspace}
\alias{logspace}
\alias{logseq}
\title{Linearly Spaced Sequences}
\description{
  Generate (log-)linearly spaced sequences.
}
\usage{
  linspace(x1, x2, n = 100)
  logspace(x1, x2, n = 50)
  logseq(x1, x2, n = 100)
}
\arguments{
  \item{x1}{numeric scalar specifying starting point}
  \item{x2}{numeric scalar specifying ending point}
  \item{n}{numeric scalar specifying number of points to be generated}
}
\details{
  These functions will generate linearly, logarithmically resp.
  exponentially spaced points between \code{x1} and \code{x2} resp.
  \code{10^x1} and \code{10^x2}.

  If \eqn{n < 2}, the result will be the ending point \code{x2}. For
  \code{logspace()}, if \code{x2 = pi}, the endpoint will be \code{pi}
  and not \code{10^pi}!
}
\value{
  vector containing \code{n} points between \code{x1} and \code{x2} inclusive. }
\seealso{
  \code{\link{seq}}
}
\examples{
linspace(1, 10, 9)
logspace(1, pi, 36)
logseq(0.05, 1, 20)
}
\keyword{ manip }

