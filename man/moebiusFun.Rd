\name{moebiusFun}
\alias{moebiusFun}
\alias{mertensFun}
\title{Moebius Function}
\description{
  The classical Moebius and Mertens functions in number theory.
}
\usage{
moebiusFun(n)
mertensFun(n)
}
\arguments{
  \item{n}{Positive integer.}
}
\details{
  \code{moebiusFun(n)} is \code{+1} if n is a square-free positive integer
  with an even number of prime factors, or \code{+1} if there are an odd
  of prime factors. It is \code{0} if \code{n} is not square-free.

  \code{mertensFun(n)} is the aggregating summary function, that sums up all
  values of \code{moebius} from \code{1} to \code{n}. 
}
\value{
  For \code{moebiusFun}, \code{0, 1} or \code{-1}, depending on the prime
  decomposition of \code{n}.

  For \code{mertensFun} the values will very slowly grow.
}
\author{
  HwB <hwborchers@googlemail.com>
}
\note{
  Works well up to \code{10^9}, but will become very slow for the Mertens
  function.
}
\seealso{
\code{\link{factorize}}, \code{\link{eulersPhi}}
}
\examples{
sapply(1:16, moebiusFun)
sapply(1:16, mertensFun)

\dontrun{
x <- 1:50; y <- sapply(x, moebiusFun)
plot(c(1, 50), c(-3, 3), type="n")
grid()
points(1:50, y, pch=18, col="blue")

x <- 1:100; y <- sapply(x, mertensFun)
plot(c(1, 100), c(-5, 3), type="n")
grid()
lines(1:100, y, col="red", type="s")}
}
\keyword{ math }
