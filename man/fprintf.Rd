\name{fprintf}
\alias{fprintf}
\title{
  Formatted Printing (Matlab style)
}
\description{
  Formatted printing to stdout or a file.
}
\usage{
fprintf(fmt, ..., file = "", append = FALSE)
}
\arguments{
  \item{fmt}{a character vector of format strings.}
  \item{...}{values passed to the format string.}
  \item{file}{a connection or a character string naming the file to
              print to; default is "" which means standard output.}
  \item{append}{logical; shall the output be appended to the file;
                default is \code{FALSE}.}
}
\details{
  \code{fprintf} applies the format string \code{fmt} to all input
  data \code{...} and writes the result to standard output or a file.
  The usual C-style string formatting commands are used-
}
\value{
  Returns invisibly the number of bytes printed (using \code{nchar}).
}
\seealso{
  \code{\link{sprintf}}
}
\examples{
##  Examples:
nbytes <- fprintf("Results are:\n", file = "")
for (i in 1:10) {
    fprintf("\%4d  \%15.7f\n", i, exp(i), file = "")
}
}
\keyword{ utilities }
