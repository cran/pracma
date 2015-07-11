\name{disp,beep}
\alias{disp}
\alias{beep}
\title{
Utility functions (Matlab style)
}
\description{
  Display text or array, or produce beep sound.
}
\usage{
disp(...)
beep()
}
\arguments{
  \item{...}{any R object that can be printed.}
}
\details{
  Display text or array, or produces the computer's default beep sound
  using `cat' with closing newline.
}
\value{
  beep() returns NULL invisibly, disp() displays with newline.
}
\examples{
disp("Some text, and numbers:", pi, exp(1))
# beep()
}
\keyword{ utilities }
