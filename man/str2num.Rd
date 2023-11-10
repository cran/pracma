\name{str2num}
\alias{str2num}
\alias{num2str}
\title{
  Converting string to number (Matlab style)
}
\description{
  Functions for converting strings to numbers and numbers to strings.
}
\usage{
str2num(S)
num2str(A, fmt = 3)
}
\arguments{
  \item{S}{string containing numbers (in Matlab format).}
  \item{A}{numerical vector or matrix.}
  \item{fmt}{format string, or integer indicating number of decimals.}
}
\details{
  \code{str2num} converts a string containing numbers into a numerical object.
  The string can begin and end with '[' and ']', numbers can be separated with
  blanks or commas; a semicolon within the brackets indicates a new row for
  matrix input. When a semicolon appears behind the braces, no output is shown
  on the command line.

  \code{num2str} converts a numerical object, vector or matrix, into a 
  character object of the same size. \code{fmt} will be a format string for
  use in \code{sprintf}, or an integer \code{n} being used in \code{'\%.nf'}.
}
\value{
  Returns a vector or matrix of the same size, converted to strings,
  respectively numbers.
}
\seealso{
  \code{\link{sprintf}}
}
\examples{
str1 <- " [1 2 3; 4, 5, 6; 7,8,9]  "
str2num(str1)
# matrix(1:9, nrow = 3, ncol = 3, byrow = TRUE)

# str2 <- " [1 2 3; 45, 6; 7,8,9]  "
# str2num(str2)
# Error in str2num(str2) : 
#   All rows in Argument 's' must have the same length.

A <- matrix(c(pi, 0, exp(1), 1), 2, 2)
B <- num2str(A, 2); b <- dim(B)
B <- as.numeric(B); dim(B) <- b
B
#      [,1] [,2]
# [1,] 3.14 2.72
# [2,] 0.00 1.00
}
\keyword{ manip }
