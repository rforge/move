\name{trackId}

\alias{trackId}
\alias{trackId,.MoveTrackStack-method}
\alias{trackId,MoveStack-method}

\docType{methods}

\title{Returns trackId}

\description{Obtain a factor returning the ids of individuals per location}

\usage{
  \S4method{trackId}{MoveStack}(x)
}

\arguments{
  \item{x}{a MoveStack object}
}

\value{
	Returns a factor indicating to which individuals locations belong in a stack.
}

\author{Bart Kranstauber}

\examples{
  load(system.file("extdata", "move.RData", package="move"), .GlobalEnv)
  stack <- moveStack(list(leroy,leroy))
  trackId(stack)
  
}
