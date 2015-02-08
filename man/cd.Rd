\name{cd, pwd, what}
\alias{cd}
\alias{pwd}
\alias{what}
\title{
Directory Functions (Matlab style)
}
\description{
  Displays or changes working directory, or lists files therein.
}
\usage{
cd(dname)
pwd()

what(dname = getwd())
}
\arguments{
  \item{dname}{(relative or absolute) directory path.}
}
\details{
  \code{pwd()} displays the name of the current directory, and is the same
  as \code{cd()}. \code{cd(dname)} changes to directory \code{dname} and if
  successfull displays the directory name.

  \code{what()} lists all files in a directory.\cr
}
\value{
  Name of the current working directory.
}
\seealso{
  \code{\link{getwd}}, \code{\link{setwd}}, \code{\link{list.files}}
}
\examples{
# cd()
# pwd()
# what()
}
\keyword{ utilities }
