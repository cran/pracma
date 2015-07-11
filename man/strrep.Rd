\name{strRep}
\alias{strRep}
\title{
  Find and replace substring
}
\description{
  Find and replace all occurrences of a substring with another one
  in all strings of a character vector.
}
\usage{
strRep(s, old, new)
}
\arguments{
  \item{s}{Character vector.}
  \item{old}{String to be replaced.}
  \item{new}{String that replaces another one.}
}
\details{
  Replaces all occurrences of \code{old} with \code{new} in all strings
  of character vector \code{s}. The matching is case sensitive.
}
\value{
  A character vector of the same length.
}
\seealso{
\code{\link{gsub}}, \code{regexprep}
}
\examples{
S <- c('This is a good example.', "He has a good character.",
       'This is good, good food.', "How goodgood this is!")
strRep(S, 'good', 'great')
}
\keyword{ string }
