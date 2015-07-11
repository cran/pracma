\name{ifft}
\alias{ifft}
\alias{ifftshift}
\alias{fftshift}
\title{
  Inverse Fast Fourier Transformation
}
\description{
  Performs the inverse Fast Fourier Transform.
}
\usage{
ifft(x)

ifftshift(x)
fftshift(x)
}
\arguments{
  \item{x}{a real or complex vector}
}
\details{
  \code{ifft} returns the value of the normalized discrete, univariate,
  inverse Fast Fourier Transform of the values in \code{x}.

  \code{ifftshift} and \code{fftshift} shift the zero-component to the center
  of the spectrum, that is swap the left and right half of \code{x}.
}
\value{
  Real or complex vector of the same length.
}
\note{
  Almost an alias for R's \code{fft(x, inverse=TRUE)}, but dividing by
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

##  Compute the derivative: F(df/dt) = (1i*k) * F(f)
#   hyperbolic secans f <- sech
df <- function(x) -sech(x) * tanh(x)
d2f <- function(x) sech(x) - 2*sech(x)^3
L <- 20                                 # domain [-L/2, L/2]
N <- 128                                # number of Fourier nodes
x <- linspace(-L/2, L/2, N+1)           # domain discretization
x <- x[1:N]                             # because of periodicity
dx <- x[2] - x[1]                       # finite difference
u <- sech(x)                            # hyperbolic secans
u1d <- df(x); u2d <- d2f(x)             # first and second derivative
ut <- fft(u)                            # discrete Fourier transform
k <- (2*pi/L)*fftshift((-N/2):(N/2-1))  # shifted frequencies
u1 <- Re(ifft((1i*k) * ut))             # inverse transform
u2 <- Re(ifft(-k^2 * ut))               # first and second derivative
\dontrun{
plot(x, u1d, type = "l", col = "blue")
points(x, u1)
grid()
figure()
plot(x, u2d, type = "l", col = "darkred")
points(x, u2)
grid()}
}
\keyword{ array }



