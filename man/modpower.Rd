\name{modpower}
\alias{modpower}
\alias{modorder}
\title{
  Power Function modulo m
}
\description{
  Calculates powers and orders modulo \code{m}.
}
\usage{
modpower(n, k, m)
modorder(n, m)
}
\arguments{
  \item{n, k, m}{Natural numbers, \code{m >= 1}.}
}
\details{
  \code{modpower} calculates \code{n} to the power of \code{k} modulo
  \code{m}.\cr
  Uses modular exponentiation, as described in the Wikipedia article.

  \code{modorder} calculates the order of \code{n} in the multiplicative
  group module \code{m}. \code{n} and \code{m} must be coprime.\cr
  Uses brute force, trick to use binary expansion and square is not more
  efficient in an R implementation.
}
\value{
  Natural number.
}
\note{
  This function is \emph{not} vectorized.
}
\seealso{
  \code{\link{primroot}}
}
\examples{
modpower(2, 100, 7)  #=> 2
modpower(3, 100, 7)  #=> 4
modorder(7, 17)      #=> 16, i.e. 7 is a primitive root mod 17

##  Gauss' table of primitive roots modulo prime numbers < 100
proots <- c(2,  2,  3,  2,  2,  6,  5, 10, 10, 10, 2,  2, 10, 17,  5,  5,
            6, 28, 10, 10, 26, 10, 10,  5, 12, 62, 5, 29, 11, 50, 30, 10)
P <- primes(100)
for (i in seq(along=P)) {
    cat(P[i], "\t", modorder(proots[i], P[i]), proots[i], "\t", "\n")
}

\dontrun{
##  Lehmann's primality test
lehmann_test <- function(n, ntry = 25) {
    if (!is.numeric(n) || ceiling(n) != floor(n) || n < 0)
        stop("Argument 'n' must be a natural number")
    if (n >= 9e7)
        stop("Argument 'n' should be smaller than 9e7.")

    if (n < 2)                      return(FALSE)
    else if (n == 2)                return(TRUE)
    else if (n > 2 && n \%\% 2 == 0)  return(FALSE)

    k <- floor(ntry)
    if (k < 1) k <- 1
    if (k > n-2) a <- 2:(n-1)
    else         a <- sample(2:(n-1), k, replace = FALSE) 

    for (i in 1:length(a)) {
        m <- modpower(a[i], (n-1)/2, n)
        if (m != 1 && m != n-1) return(FALSE)
    }
    return(TRUE)
}

##  Examples
for (i in seq(1001, 1011, by = 2))
    if (lehmann_test(i)) cat(i, "\n")
# 1009
system.time(lehmann_test(27644437, 50))    # TRUE
#    user  system elapsed 
#   0.086   0.151   0.235
}
}
\keyword{ math }
