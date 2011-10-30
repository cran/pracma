\name{eulersPhi}
\alias{eulersPhi}
\title{Eulers's Phi Function}
\description{
  Euler's Phi function (aka Euler's `totient' function).
}
\usage{
eulersPhi(n)
}
\arguments{
  \item{n}{Positive integer.}
}
\details{
  The \code{phi} function is defined to be the number of positive integers
  less than or equal to \code{n} that are \emph{coprime} to \code{n}, i.e.
  have no common factors other than 1.
}
\value{
  Natural number, the number of coprime integers \code{<= n}.
}
\author{
  HwB <hwborchers@googlemail.com>
}
\note{
  Works well up to \code{10^9}.
}
\seealso{
\code{\link{factorize}}, \code{\link{sigma}}
}
\examples{
eulersPhi(9973)  == 9973 - 1                       # for prime numbers
eulersPhi(3^10)  == 3^9 * (3 - 1)                  # for prime powers
eulersPhi(12*35) == eulersPhi(12) * eulersPhi(35)  # TRUE if coprime

\dontrun{
x <- 1:100; y <- sapply(x, eulersPhi)
plot(1:100, y, type="l", col="blue")
points(1:100, y, col="blue", pch=20)
grid()}
}
\keyword{ math }
