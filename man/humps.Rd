\name{humps}
\alias{humps}
\alias{sinc}
\title{
Matlab Test Functions
}
\description{
Matlab test functions.
}
\usage{
humps(x)
sinc(x)
}
\arguments{
  \item{x}{Numeric scalar or vector.}
}
\details{
  \code{humps} is a test function for finding zeros, for optimization and
  integration. Its root is at \code{x = 1.2995}, a (local) minimum at
  \code{x = 0.6370}, and the integral from \code{0.5} to \code{1.0} is
  \code{8.0715}.

  \code{sinc} is defined as \eqn{sinc(t) = \frac{\sin(\pi t)}{\pi t}}.
  It is the continuous inverse Fourier transform of the rectangular pulse
  of width \eqn{2\pi} and height \eqn{1}.
}
\value{
  Numeric scalar or vector.
}
\examples{
\dontrun{
plot(humps(), type="l"); grid()

x <- seq(0, 10, length=101)
plot(x, sinc(x), type="l"); grid()
}
}
