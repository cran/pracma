\name{rat}
\alias{rat}
\alias{rats}
\title{
  Continuous Fractions (Matlab Style)
}
\description{
  Generate continuous fractions for numeric values.
}
\usage{
rat(x, tol = 1e-06)
rats(x, tol = 1e-06)
}
\arguments{
  \item{x}{a numeric scalar or vector.}
  \item{tol}{tolerance; default \code{1e-6} to make a nicer appearance for
             \code{pi}.}
}
\details{
  \code{rat} generates continuous fractions, while \code{rats} prints the
  the corresponding rational representation and returns the numeric values.
}
\value{
  \code{rat} returns a character vector of string representations of
  continuous fractions in the format \code{[b0; b1, ..., b_{n-1}]}.

  \code{rats} prints the rational number and returns a numeric vector.
}
\note{
  Essentially, these functions apply \code{contfrac}.
}
\seealso{
\code{numbers::contfrac}
}
\examples{
rat(pi)
rats(pi)
rat(sqrt(c(2, 3, 5)),  tol = 1e-15)
rats(sqrt(c(2, 3, 5)), tol = 1e-15)
}
\keyword{ math }
