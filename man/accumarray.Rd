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
accumarray(subs, val, sz = NULL, func = sum, fillval = 0)

uniq(a, first = FALSE)
}
\arguments{
  \item{subs}{vector or matrix of positive integers,
              used as indices for the result vector.}
  \item{val}{numerical vector.}
  \item{sz}{size of the resulting array.}
  \item{func}{function to be applied to a vector of numbers.}
  \item{fillval}{value used to fill the array when there are no indices
                 pointing to that component.}
  \item{a}{numerical vector.}
  \item{first}{logical, shall the first or last element encountered be used.}
}
\details{
  \code{A <- accumarray(subs, val)} creates an array \code{A} by accumulating
  elements of the vector \code{val} using the lines of \code{subs} as indices
  and applying \code{func} to that accumulated vector. The size of the array
  can be predetermined by the size vector \code{sz}.

  \code{A = uniq(a)} returns a vector \code{b} identical to \code{unique(a)}
  and two other vectors of indices \code{m} and \code{n} such that
  \code{b == a[m]} and \code{a == b[n]}.
}
\value{
\code{accumarray} returns an array of size the maximum in each column of 
\code{subs}, or by \code{sz}.

\code{uniq} returns a list with components

\item{ b }{vector of unique elements of a.}
\item{ m }{vector of indices such that \code{b = a[m]}}
\item{ n }{vector of indices such that \code{a = b[n]}}
}
\note{
  The Matlab function \code{accumarray} can also handle sparse matrices.
}
\seealso{
  \code{\link{unique}}
}
\examples{
##  Examples for accumarray
val = 101:105
subs = as.matrix(c(1, 2, 4, 2, 4))
accumarray(subs, val)
# [101; 206; 0; 208]

val = 101:105
subs <- matrix(c(1,2,2,2,2, 1,1,3,1,3, 1,2,2,2,2), ncol = 3)
accumarray(subs, val)
# , , 1
# [,1] [,2] [,3]
# [1,]  101    0    0
# [2,]    0    0    0
# , , 2
# [,1] [,2] [,3]
# [1,]    0    0    0
# [2,]  206    0  208

val = 101:106
subs <- matrix(c(1, 2, 1, 2, 3, 1, 4, 1, 4, 4, 4, 1), ncol = 2, byrow = TRUE)
accumarray(subs, val, func = function(x) sum(diff(x)))
# [,1] [,2] [,3] [,4]
# [1,]    0    1    0    0
# [2,]    0    0    0    0
# [3,]    0    0    0    0
# [4,]    2    0    0    0

val = 101:105
subs = matrix(c(1, 1, 2, 1, 2, 3, 2, 1, 2, 3), ncol = 2, byrow = TRUE)
accumarray(subs, val, sz = c(3, 3), func = max, fillval = NA)
# [,1] [,2] [,3]
# [1,]  101   NA   NA
# [2,]  104   NA  105
# [3,]   NA   NA   NA

##  Examples for uniq
a <- c(1, 1, 5, 6, 2, 3, 3, 9, 8, 6, 2, 4)
A <- uniq(a); A
# A$b  1  5  6  2  3  9  8  4
# A$m  2  3 10 11  7  8  9 12
# A$n  1  1  2  3  4  5  5  6  7  3  4  8
A <- uniq(a, first = TRUE); A
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
A <- allsums(seq(1, 9, by=2)); A
# A$S  1  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 24 25
# A$C  1  1  1  1  1  1  2  2  2  1  2  2  1  2  2  2  1  1  1  1  1  1  1
}
\keyword{ array }
