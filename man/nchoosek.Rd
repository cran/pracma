\name{nchoosek}
\alias{nchoosek}
\title{
Binomial Coefficients
}
\description{
  Compute the Binomial coefficients.
}
\usage{
nchoosek(n, k)
}
\arguments{
  \item{n, k}{integers with \code{k} between 0 and \code{n}}
}
\details{
  Alias for the corresponding R function \code{choose}.
}
\value{
  integer, the Binomial coefficient \eqn{({n \over k})}.
}
\note{
  In Matlab/Octave, if \code{n} is a vector all combinations of \code{k}
  elements from vector \code{n} will be generated. Here, use the function
  \code{combs} instead.
}
\seealso{
\code{\link{choose}}
}
\examples{
S <- sapply(0:6, function(k) nchoosek(6, k))  # 1  6 15 20 15  6  1

# Catalan numbers
catalan <- function(n) choose(2*n, n)/(n+1)
catalan(0:10)
# 1  1  2  5  14  42  132  429  1430  4862  16796

# Relations
n <- 10
sum((-1)^c(0:n) * sapply(0:n, function(k) nchoosek(n, k)))  # 0
}
\keyword{ arith }
