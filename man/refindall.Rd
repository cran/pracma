\name{refindall}
\alias{refindall}
\title{
  Find overlapping regular expression matches.
}
\description{
  Find overlapping matches for a regular expression.
}
\usage{
refindall(s, pat, over = 1, ignorecase = FALSE)
}
\arguments{
\item{s}{Single character string.}
\item{pat}{Regular expression.}
\item{over}{Natural number, indication how many steps to go forward
            after a match; defaults to 1.}
\item{ignorecase}{logical, whether to ignore case.}
}
\details{
  Returns the starting position of all --- even overlapping --- matches
  of the regular expression \code{pat} in the character string \code{s}.

  The syntax for pattern matching has to be PERL-like.
}
\value{
  A numeric vector with the indices of starting positions of all matches.
}
\note{
  This effect can also be reached with the R function gregexpr(), see the
  example below.
}
\seealso{
\code{\link{regexp}}
}
\examples{
refindall("ababababa", 'aba')
gregexpr('a(?=ba)', "ababababa", perl=TRUE)

refindall("AbababaBa", 'aba')
refindall("AbababaBa", 'aba', ignorecase = TRUE)
}
\keyword{ string }
