\name{rmserr}
\alias{rmserr}
\title{
  Accuracy Measures
}
\description{
  Calculates different accuracy measures, most prominently RMSE.
}
\usage{
rmserr(x, y, summary = FALSE)
}
\arguments{
  \item{x, y}{two vectors of real numbers}
  \item{summary}{logical; should a summary be printed to the screen?}
}
\details{
  Calculates six different measures of accuracy for two given vectors or
  sequences of real numbers:

  \tabular{ll}{
  MAE  \tab Mean Absolute Error\cr
  MSE  \tab Mean Squared Error\cr
  RMSE \tab Root Mean Squared Error\cr
  MAPE \tab Mean Absolute Percentage Error\cr
  LMSE \tab Normalized Mean Squared Error\cr
  rSTD \tab relative Standard Deviation
  }
}
\value{
  Returns a list with different accuracy measures.
}
\references{
  Gentle, J. E. (2009). Computational Statistics, section 10.3.
  Springer Science+Business Media LCC, New York.
}
\note{
  Often used in Data Mining for \emph{predicting} the accuracy of predictions.
}
\examples{
x <- rep(1, 10)
y <- rnorm(10, 1, 0.1)
rmserr(x, y, summary = TRUE)
}
\keyword{ stat }
