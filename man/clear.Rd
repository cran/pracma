\name{clear, who}
\alias{clear}
\alias{who}
\title{
Clear function (Matlab style)
}
\description{
  List or remove items from workspace.
}
\usage{
clear(lst)
who()
}
\arguments{
  \item{lst}{Character vector of names of variables in the global environment.}
}
\details{
  Remove these or all items from the workspace, i.e. the global environment,
  and freeing up system memory.

  \code{who} lists all items on the workspace.
}
\value{
  Invisibly NULL.
}
\seealso{
  \code{\link{ls}}, \code{\link{rm}}
}
\examples{
who()
# clear()  # DON'T
}
