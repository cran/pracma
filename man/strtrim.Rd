\name{strtrim}
\alias{strtrim}
\alias{deblank}
\title{
  Remove leading and trailing white space.
}
\description{
  Removes leading and trailing white space from a string.
}
\usage{
strtrim(s)
deblank(s)
}
\arguments{
  \item{s}{character string or character vector}
}
\details{
  \code{strtrim} removes leading and trailing white space from a string or
  from all strings in a character vector.

  \code{deblank} removes trailing white space only from a string or
  from all strings in a character vector.  
}
\value{
  A character string or character vector with (leading and) trailing
  white space.
}
\seealso{
\code{\link{strjust}}
}
\examples{
s <- c("  abc", "abc   ", " abc ", " a b c ", "abc", "a b c")
strtrim(s)
deblank(s)
}
\keyword{ string }
