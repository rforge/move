\name{split}

\alias{split}
\alias{split,MoveStack,missing-method}
\alias{split,.MoveTrackSingleBurst,missing-method}
\alias{split,DBBMMStack,missing-method}

\docType{methods}

\title{Splitting a MoveStack}

\description{Splitting MoveStack in a list of Move objects}

\usage{
  \S4method{split}{MoveStack,missing}(x, f, drop=FALSE, ...)
}

\arguments{
  \item{x}{MoveStack object}
  \item{f}{not needed}
  \item{drop}{not needed}
  \item{...}{not needed}
}

\details{
  A MoveStack is split into a list of \link[=Move-class]{Move} objects by the track IDs of the given MoveStack. 
  For stacking \link{moveStack} can be used. 
}

\author{Marco Smolla}

\examples{
load(system.file("extdata", "move.RData", package="move"), .GlobalEnv)
movestack <- moveStack(list(leroy,ricky))
#dbbmmstack <- brownian.bridge.dyn(spTransform(stack), location.error=23.5, dimSize=85, time.step=6000)

split(movestack) #splitting a MoveStack
split(dbbmmstack) #splitting a DBBMMStack
}
