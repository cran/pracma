\name{sorting}
\alias{bubbleSort}
\alias{insertionSort}
\alias{selectionSort}
\alias{shellSort}
\alias{heapSort}
\alias{mergeSort}
\alias{mergeOrdered}
\alias{quickSort}
\alias{quickSortx}
\alias{is.sorted}
\alias{testSort}
\title{Sorting Routines}
\description{
  R implementations of several sorting routines. These implementations are
  meant for demonstration and lecturing purposes.
}
\usage{
is.sorted(a)
testSort(n = 1000)

bubbleSort(a)
insertionSort(a)
selectionSort(a)
shellSort(a, f = 2.3)
heapSort(a)
mergeSort(a, m = 10)
mergeOrdered(a, b)
quickSort(a, m = 3)
quickSortx(a, m = 25)
}
\arguments{
  \item{a, b}{Numeric vectors to be sorted or merged.}
  \item{f}{Retracting factor for \code{shellSort}.}
  \item{m}{Size of subsets that are sorted by \code{insertionSort} when the
           sorting procedure is called recursively.}
  \item{n}{Only in \code{testSort}: the length of a vector of random numbers
           to be sorted.}
}
\details{
  \code{bubbleSort(a)} is the well-known ``bubble sort'' routine; it is
  forbiddingly slow.

  \code{insertionSort(a)} sorts the array one entry at a time; it is slow,
  but quite efficient for small data sets.

  \code{selectionSort(a)} is an in-place sorting routine that is inefficient,
  but noted for its simplicity.

  \code{shellSort(a, f = 2.3)} exploits the fact that insertion sort works
  efficiently on input that is already almost sorted. It reduces the gaps
  by the factor \code{f}; \code{f=2.3} is said to be a reasonable choice.

  \code{heapSort(a)} is not yet implemented.

  \code{mergeSort(a, m = 10)} works recursively, merging already sorted parts
  with \code{mergeOrdered}. \code{m} should be between\code{3} and 1/1000 of
  the size of \code{a}.

  \code{mergeOrdered(a, b)} works only correctly if \code{a} and \code{a}
  are already sorted.

  \code{quickSort(a, m = 3)} realizes the celebrated ``quicksort algorithm''
  and is the fastest of all implementations here. To avoid too deeply nested
  recursion with R, \code{insertionSort} is called when the size of a subset
  is smaller than \code{m}.
  
  Values between \code{3..30} seem reasonable and smaller values are better,
  with the risk of running into a too deeply nested recursion.

  \code{quickSort(a, m = 25)} is an extended version where the split is
  calculated more carefully, but in general this approach takes too much
  time.

  Values for \code{m} are \code{20..40} with \code{m=25} favoured.

  \code{testSort(n = 1000)} is a test routine, e.g. for testing your
  computer power. On an iMac, \code{quickSort} will sort an array of
  size 1,000,000 in less than 15 secs.
}
\value{
  All routines return the vector sorted.

  \code{is.sorted} indicates logically whether the vector is sorted.
}
\references{
  Knuth, D. E. (1973). The Art of Computer Programming, Volume 3: Sorting
  and Searching, Chapter 5: Sorting. Addison-Wesley Publishing Company.
}
\author{
  HwB <hwborchers@googlemail.com>
}
\note{
  At the moment, only increasingly sorting is possible
  (if needed apply \code{rev} afterwards).
}
\seealso{
\code{\link{sort}}, the internal C-based sorting routine.
}
\examples{
\dontrun{
testSort(100)

a <- sort(runif(1000)); b <- sort(runif(1000))
system.time(y <- mergeSort(c(a, b)))
system.time(y <- mergeOrdered(a, b))
is.sorted(y)}
}
\keyword{ array }
