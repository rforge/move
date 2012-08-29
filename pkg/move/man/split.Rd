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
  A MoveStack is splitted into a list of \link[=Move-class]{Move} objects by the trackIds of the given MoveStack. 
  To stack this list again \link{moveStack} can be used. 
}

\author{Marco Smolla}

\examples{
isaac <- move(system.file("extdata","leroy.csv", package="move"))[1:300,]
isaac2 <- move(system.file("extdata","leroy.csv", package="move"))[400:600,]
moveStack <- moveStack(list(isaac, isaac2))

split(moveStack) #splitting a MoveStack
dbbmmStack <- brownian.bridge.dyn(spTransform(moveStack), location.error=23.5, dimSize=85, time.step=6000)

split(dbbmmStack) #splitting a DBBMMStack
}
