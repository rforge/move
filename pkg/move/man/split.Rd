\name{split}

\alias{split}
\alias{split,MoveStack,missing-method}
\alias{split,.MoveTrackSingleBurst,missing-method}
\alias{split,.MoveTrackStack,missing-method}
\alias{split,DBBMMStack,missing-method}
\alias{split,.UDStack,missing-method}

\docType{methods}

\title{Splitting a MoveStack}

\description{Splitting MoveStack in a list of Move objects}

\usage{
  \S4method{split}{MoveStack,missing}(x, f, drop=FALSE, ...)
}

\arguments{
  \item{x}{MoveStack or MoveBurst object}
  \item{f}{not needed}
  \item{drop}{not needed}
  \item{...}{not needed}
}

\details{
  A MoveStack is split into a list of \link[=Move-class]{Move} objects by the track IDs of the given MoveStack. 
  For stacking \link{moveStack} can be used. Also a \link[=MoveBurst-class]{MoveBurst} object can be split this will lead to seperate list elements for every burst, every location where the burst is switched will then be recycled.
}

\author{Marco Smolla}

\examples{
data(dbbmmstack)
data(fishers)
split(fishers) #splitting a MoveStack
split(dbbmmstack) #splitting a DBBMMStack
}
