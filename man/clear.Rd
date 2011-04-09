\name{clear}
\alias{clear}
\title{
Clear function (Matlab style)
}
\description{
  Remove items from workspace.
}
\usage{
clear(lst)
}
\arguments{
  \item{lst}{Character vector of names of variables in the global environment.}
}
\details{
  Remove these or all items from the workspace, i.e. the global environment,
  and freeing up system memory.
}
\value{
  Invisibly NULL.
}
\seealso{
  \code{\link{rm}}
}
\examples{
# clear()  # DONT
}
