\name{clear, who}
\alias{clear}
\alias{what}
\alias{who}
\title{
Clear function (Matlab style)
}
\description{
  List or remove items from workspace.
}
\usage{
clear(lst)

what(dname = getwd())
who()
}
\arguments{
  \item{lst}{Character vector of names of variables in the global environment.}
  \item{dname}{(relative or absolute) directory path.}
}
\details{
  Remove these or all items from the workspace, i.e. the global environment,
  and freeing up system memory.

  \code{who} lists all items on the workspace.
  \code{what} lists all files in a directory.
}
\value{
  Invisibly NULL.
}
\seealso{
  \code{\link{ls}}, \code{\link{rm}}
}
\examples{
# clear()  # DON'T
# what()
# who()
}
