\name{poly2str}
\alias{poly2str}
\title{
  Print Polynomial
}
\description{
  Print polynomial as a character string.
}
\usage{
poly2str(p, svar = "x", smul = "*", d = options("digits")$digits)
}
\arguments{
  \item{p}{numeric vector representing a polynomial}
  \item{svar}{character representing the unknown, default \code{x}.}
  \item{smul}{multiplication symbol, default \code{*}.}
  \item{d}{significant digits, default \code{options("digits")}.}
}
\details{
  Simple string manipulation.
}
\value{
  Returns the usual string representing a polynomial in mathematics.
}
\examples{
poly2str(c(0))
poly2str(c(1, -1, 1, -1, 1))
poly2str(c(0, 1e-6, 1e6), d = 2)
}
\keyword{ array }
