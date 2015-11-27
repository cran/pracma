\name{strfind}
\alias{strfind}
\alias{strfindi}
\alias{findstr}
\title{Find Substrings}
\description{
  Find substrings within strings of a character vector.
}
\usage{
strfind(s1, s2, overlap = TRUE)
strfindi(s1, s2, overlap = TRUE)

findstr(s1, s2, overlap = TRUE)
}
\arguments{
\item{s1}{character string or character vector}
\item{s2}{character string (character vector of length 1)}
\item{overlap}{logical (are overlapping substrings allowed)}
}
\details{
  \code{strfind} finds positions of substrings within \code{s1} that
  match exactly with \code{s2}, and is case sensitive; no regular patterns.

  \code{strfindi} does not distinguish between lower and upper case.

  \code{findstr} should only be used as internal function, in Matlab it is
  deprecated. It searches for the shorter string within the longer one.
}
\value{
  Returns a vector of indices, or a list of such index vectors if
  \code{s2} is a character vector of length greater than 1.
}
\seealso{
\code{\link{strcmp}}
}
\examples{
S <- c("", "ab", "aba", "aba aba", "abababa")
s <- "aba"
strfind(S, s)
strfindi(toupper(S), s)
strfind(S, s, overlap = FALSE)
}
\keyword{ string }
