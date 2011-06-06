\name{ifft}
\alias{ifft}
\title{
  Inverse Fast Fourier Transformation
}
\description{
  Performs the inverse Fast Fourier Transform.
}
\usage{
ifft(x)
}
\arguments{
  \item{x}{a real or complex vector}
}
\details{
  Returns the value of the normalized discrete, univariate, inverse Fast
  Fourier Transform of the values in \code{x}.
}
\value{
  Real or complex vector of the same length.
}
\note{
  Almost an alias for \code{fft(x, inverse=TRUE)}, but dividing by
  \code{length(x)}.
}
\seealso{
  \code{\link{fft}}
}
\examples{
x <- c(1, 2, 3, 4)
(y <- fft(x))
ifft(x)
ifft(y)
}
\keyword{ array }
