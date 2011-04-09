\name{regexp}
\alias{regexp}
\alias{regexpi}
\title{
  Match regular expression
}
\description{
  Returns the positions of substrings that match the regular expression.
}
\usage{
regexp(s, pat, ignorecase = FALSE, once = FALSE, split = FALSE)

regexpi(s, pat, once = FALSE, split = FALSE)
}
\arguments{
  \item{s}{Character string, i.e. of length 1.}
  \item{pat}{Matching pattern as character string.}
  \item{ignorecase}{Logical: whether case should be ignored;
        default: \code{FALSE}.}
  \item{once}{Logical: whether the first are all occurrences should be found;
        default: all.}
  \item{split}{Logical: should the string be splitted at the occurrences of
               the pattern?; default: no.}
}
\details{
  Returns the start and end positions and the exact value of substrings
  that match the regular expression. If \code{split} is choosen, the
  splitted strings will also be returned.
}
\value{
  A list with components \code{start} and \code{end} as numeric vectors
  indicating the start and end positions of the matches.

  \code{match} contains each exact match, and \code{split} contains the
  character vector of splitted strings.

  If no match is found all components will be \code{NULL}, except
  \code{split} that will contain the whole string if \code{split = TRUE}.
}
\note{
  This is the behavior of the corresponding Matlab function, though the
  signature, options and return values do not match exactly.
  Notice the transposed parameters \code{s} and \code{pat} compared to the
  corresponding R function \code{regexpr}.
}
\seealso{
\code{\link{regexpr}}
}
\examples{
s <- "bat cat can car COAT court cut ct CAT-scan"
pat <-  'c[aeiou]+t'
regexp(s, pat)
regexpi(s, pat)
}
\keyword{ string }
