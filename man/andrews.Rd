\name{andrewsplot}
\alias{andrewsplot}
\title{
  Andrews' Curves
}
\description{
  Plots Andrews' curves in cartesian or polar coordinates.
}
\usage{
andrewsplot(A, f, style = "pol", scaled = FALSE, npts = 101)
}
\arguments{
  \item{A}{numeric matrix with at least two columns.}
  \item{f}{factor or integer vector with \code{nrow(A)} elements.}
  \item{style}{character variable, only possible values `cart' or `pol'.}
  \item{scaled}{logical; if true scales each column to have mean 0 and
                standard deviation 1 (not yet implemented).}
  \item{npts}{number of points to plot.}
}
\details{
  \code{andrewsplot} creates an Andrews plot of the multivariate data in the
  matrix \code{A}, assigning different colors according to the factor or
  integer vector \code{f}.

  Andrews' plot represent each observation (row) by a periodic function over
  the interval \code{[0, 2*pi]}. This function for the \code{i}-th observation
  is defined as ...

  The plot can be seen in cartesian or polar coordinates --- the latter seems
  appropriate as all these functions are periodic.
}
\value{
  Generates a plot, no return value.
}
\note{
  Please note that a different ordering of the columns will result in quite
  different functions and overall picture.

  There are variants utilizing principal component scores, in order of
  decreasing eigenvalues.
}
\references{
  R. Khattree and D. N. Naik (2002). Andrews PLots for Multivariate Data:
  Some New Suggestions and Applications. Journal of Statistical Planning
  and Inference, Vol. 100, No. 2, pp. 411--425.
}
\seealso{
  \code{\link{polar}}, \code{andrews::andrews}
}
\examples{
\dontrun{
data(iris)
s <- sample(1:4, 4)
A <- as.matrix(iris[, s])
f <- as.integer(iris[, 5])
andrewsplot(A, f, style = "pol")
}
}
\keyword{ graphs }
