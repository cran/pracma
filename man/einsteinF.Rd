\name{einsteinF}
\alias{einsteinF}
\title{
  Einstein Functions
}
\description{
  Einstein functions.
}
\usage{
einsteinF(d, x)
}
\arguments{
  \item{x}{numeric or complex vector.}
  \item{d}{parameter to select one of the Einstein functions E1, E2, E3, E4.}
}
\details{
  The Einstein functions are sometimes used for the Planck-Einstein
  oscillator in one degree of freedom.

  The functions are defined as:
  \deqn{E1(x) = \frac{x^2 e^x}{(e^x - 1)^2}}
  \deqn{E2(x) = \frac{x}{e^x - 1}}
  \deqn{E3(x) = ln(1 - e^{-x})}
  \deqn{E4(x) = \frac{x}{e^x - 1} - ln(1 - e^{-x})}

  \code{E1} has an inflection point as \code{x=2.34694130...}.
}
\value{
  Numeric/complex scalar or vector.
}
\examples{
\dontrun{
x1 <- seq(-4, 4, length.out = 101)
y1 <- einsteinF(1, x1)
plot(x1, y1, type = "l", col = "red",
             xlab = "", ylab = "", main = "Einstein Function E1(x)")
grid()

y2 <- einsteinF(2, x1)
plot(x1, y2, type = "l", col = "red",
             xlab = "", ylab = "", main = "Einstein Function E2(x)")
grid()

x3 <- seq(0, 5, length.out = 101)
y3 <- einsteinF(3, x3)
plot(x3, y3, type = "l", col = "red",
             xlab = "", ylab = "", main = "Einstein Function E3(x)")
grid()

y4 <- einsteinF(4, x3)
plot(x3, y4, type = "l", col = "red",
             xlab = "", ylab = "", main = "Einstein Function E4(x)")
grid()}
}
