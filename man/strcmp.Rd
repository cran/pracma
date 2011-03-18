\name{strcmp}
\alias{strcmp}
\alias{strcmpi}
\title{String Comparison}
\description{
  Compare two strings or character vectors for equality.
}
\usage{
strcmp(s1, s2)
strcmpi(s1, s2)
}
\arguments{
  \item{s1, s2}{character strings or vectors}
}
\details{
  For \code{strcmp} comparisons are case-sensitive, while for \code{strcmpi}
  the are case-insensitive. Leading and trailing blanks do count.
}
\value{
  logical, i.e. \code{TRUE} if \code{s1} and \code{s2} have the same length
  as character vectors and all elements are equal as character strings, else
  \code{FALSE}.
}
\seealso{
\code{\link{strcat}}
}
\examples{
strcmp(c("yes", "no"), c("yes", "no"))
strcmpi(c("yes", "no"), c("Yes", "No"))
}
\keyword{ string }
