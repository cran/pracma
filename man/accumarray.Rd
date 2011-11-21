\name{accumarray}
\alias{accumarray}
\alias{uniq}
\title{
  Accumulate Vector Elements
}
\description{
  \code{accumarray} groups elements from a data set and applies a function
  to each group.
}
\usage{
accumarray(is, a, func = sum)

uniq(a, first = FALSE)
}
\arguments{
  \item{is}{positive integers, used as indices for the result vector.}
  \item{a}{numerical vector.}
  \item{func}{function to be applied to a (sub)vector of numbers.}
  \item{first}{logical, shall the first or last element encountered be used.}
}
\details{
  \code{A <- accumarray(is, a)} creates a vector A by accumulating
  elements of the vector \code{a} using the elements of \code{is} as
  indices.

  The position of an element in \code{is} determines which value of
  \code{is} it selects for the accumulated vector. This works like a
  factor in core R. The value of an element in \code{is} determines the
  position of the accumulated vector in the output.

  \code{A = uniq(a)} returns a vector \code{b} identical to \code{unique(a)}
  and two other vectors of indices \code{m} and \code{n} such that
  \code{b == a[m]} and \code{a == b[n]}.
}
\value{
\code{accumarray} returns a vector of length the maximum in \code{is}.

\code{uniq} returns a list with components

\item{ $b }{vector of unique elements of a.}
\item{ $m }{vector of indices such that \code{b = a[m]}}
\item{ $n }{vector of indices such that \code{a = b[n]}}
}
\author{
  HwB  hwborchers@googlemail.com
}
\note{
  The Matlab function \code{accumarray} can also handle sparse matrices and
  pairs of indices pointing into matrices and arrays.
}
\seealso{
  \code{\link{unique}}
}
\examples{
a  <- 101:105
is <- c(1, 2, 4, 2, 4)
A <- accumarray(is, a)          # 101 206   0 208

a <- c(1, 1, 5, 6, 2, 3, 3, 9, 8, 6, 2, 4)
A <- uniq(a)
# A$b  1  5  6  2  3  9  8  4
# A$m  2  3 10 11  7  8  9 12
# A$n  1  1  2  3  4  5  5  6  7  3  4  8
A <- uniq(a, first = TRUE)
# A$m  1  3  4  5  6  8  9 12

##  Example: Subset sum problem
# Distribution of unique sums among all combinations of a vectors.
allsums <- function(a) {
    S <- c(); C <- c()
    for (k in 1:length(a)) {
        U <- uniq(c(S, a[k], S + a[k]))
        S <- U$b
        C <- accumarray(U$n, c(C, 1, C))
    }
    o <- order(S); S <- S[o]; C <- C[o]
    return(list(S = S, C = C))
}
A <- allsums(seq(1, 9, by=2))
# A$S  1  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 24 25
# A$C  1  1  1  1  1  1  2  2  2  1  2  2  1  2  2  2  1  1  1  1  1  1  1
}
\keyword{ array }
