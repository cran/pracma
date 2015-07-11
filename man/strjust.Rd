\name{strjust}
\alias{strjust}
\title{
  Justify character vector
}
\description{
  Justify the strings in a character vector.
}
\usage{
strjust(s, justify = c("left", "right", "center"))
}
\arguments{
  \item{s}{Character vector.}
  \item{justify}{Whether to justify left, right, or centered.}
}
\details{
  \code{strjust(s)} or \code{strjust(s, justify = ``right'')} returns
  a right-justified character vector. All strings have the same length,
  the length of the longest string in \code{s} --- but the strings in
  \code{s} have been trimmed before.

  \code{strjust(s, justify = ``left'')} does the same, with all strings
  left-justified.

  \code{strjust(s, justify = ``centered'')} returns all string in \code{s}
  centered. If an odd number of blanks has to be added, one blank more is
  added to the left than to the right.
}
\value{
  A character vector of the same length.
}
\seealso{
\code{\link{strTrim}}
}
\examples{
S <- c("abc", "letters", "1", "2  2")
strjust(S, "left")
}
\keyword{ string }
