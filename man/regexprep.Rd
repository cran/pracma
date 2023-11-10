\name{regexprep}
\alias{regexprep}
\title{
  Replace string using regular expression
}
\description{
  Replace string using regular expression.
}
\usage{
regexprep(s, expr, repstr, ignorecase = FALSE, once = FALSE)
}
\arguments{
\item{s}{Single character string.}
\item{expr}{Regular expression to be matched.}
\item{repstr}{String that replaces the matched substring(s).}
\item{ignorecase}{logical, whether to ignore case.}
\item{once}{logical, shall only the first or all occurences be replaced.}
}
\details{
  Matches the regular expression against the string and replaces the first
  or all non-overlapping occurrences with the replacement string.

  The syntax for regular expression has to be PERL-like.
}
\value{
  String with substrings replaced.
}
\note{
  The Matlab/Octave variant allows a character vector. This is not possible
  here as it would make the return value quite complicated.
}
\seealso{
\code{\link{gsub}}
}
\examples{
s <- "bat cat can car COAT court cut ct CAT-scan"
pat <-  'c[aeiou]+t'
regexprep(s, pat, '---')
regexprep(s, pat, '---', once = TRUE)
regexprep(s, pat, '---', ignorecase = TRUE)
}
\keyword{ string }
