\name{runs.test}
\alias{runs.test}
\title{
  The Runs Test, or Wald-Wolfowitz Test
}
\description{
  The ``Runs Test", or Wald-Wolfowitz Test for Randomness
}
\usage{
runs.test(x)
}
\arguments{
  \item{x}{vector with only two different values.}
}
\details{
  The Runs Test is a non-parametric test for checking the randomness of a 
  dichotomous sequence, i.e. with only two different values. The test counts 
  the number of `runs', subsequences consisting of the same adjacent element.

  The two characteristic elements of the sequence need not have the same 
  probability. The null hypothesis is the assumption that the elements are 
  independently drawn from from the conditional distribution given by the 
  frequncy distribution within the sequence.
}
\value{
  Returns a list with components $Z the test statistics, and $p.value the 
  p-value for the null hypothesis.
}
\references{
  The `dieharder' website at 
  \url{http://www.phy.duke.edu/~rgb/General/dieharder.php}
}
\seealso{
  \code{\link{chisq.test}}, \code{RDieHarder::dieharder}
}
\examples{
x1 <- c('H','T','T','H','H','H','T','T','T','T','T','T','T','H','H',
        'H','T','H','T','H','H','H','T','H','H','H','T','H','T','H')
runs.test(x1)
##  $Z
##  [1] -0.1617764
##  $p.value
##  [1] 0.8714819

x2 <- rep(c(0, 1), 50)
runs.test(x2)
##  $Z
##  [1] 9.849873
##  $p.value
##  [1] 0

# Which sequence is random?
x3 <- c(1,1,0,1,0,1,0,1,0,0,1,0,1,1,0,0,1,1,1,0,1,0,1,1,1,
        0,0,1,0,0,1,0,0,0,1,0,1,0,1,1,0,1,0,0,1,1,1,0,1)
x4 <- c(0,0,1,1,1,0,0,0,1,0,1,1,1,1,1,1,0,1,0,0,1,0,0,0,0,
        1,0,1,0,1,1,1,0,0,0,0,0,1,0,1,0,1,1,0,0,1,1,1,0)

runs.test(x3)   # null hypotheses rejected at 0.05 level
##  $Z
##  [1] 2.05555
##  $p.value
##  [1] 0.03982587

runs.test(x4)   # randomness is very probable
##  $Z
##  [1] 0.002947558
##  $p.value
##  [1] 0.9976482
}

\keyword{ stat }
