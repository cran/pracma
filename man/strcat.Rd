\name{strcat}
\alias{strcat}
\title{String Concatenation}
\description{
  Concatenate all strings  in a character vector
}
\usage{
strcat(s1, s2 = NULL, collapse = "")
}
\arguments{
  \item{s1}{character string or vectors}
  \item{s2}{character string or vector, or NULL (default)}
  \item{collapse}{character vector of length 1 (at best a single character)}
}
\details{
  Concatenate all strings  in character vector \code{s1}, if \code{s2} is
  \code{NULL}, or cross-concatenate all string elements in \code{s1} and
  \code{s2} using \code{collapse} as `glue'.
}
\value{
  a character string or character vector
}
\seealso{
\code{\link{paste}}
}
\examples{
strcat(c("a", "b", "c"))                        #=> "abc"
strcat(c("a", "b"), c("1", "2"), collapse="x")  #=> "ax1" "ax2" "bx1" "bx2"
}
\keyword{ string }
