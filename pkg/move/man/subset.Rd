\name{subset-method}

\alias{[,MoveStack,ANY,ANY-method}
\alias{[,.MoveTrack,ANY,ANY-method}
\alias{[,.MoveTrackSingleBurst,ANY,ANY-method}
\alias{[,.MoveTrackStack,ANY,ANY-method}
\alias{[,dBMvariance,ANY,ANY-method}
\alias{[,dBMvarianceStack,ANY,ANY-method}
\alias{[,.unUsedRecordsStack,ANY,ANY-method}
\alias{[,.unUsedRecords,ANY,ANY-method}
\alias{[[,MoveStack,character,missing-method}
\alias{[[,.MoveTrackStack,character,missing-method}

\docType{methods}

\title{Returns a single object from a MoveStack}

\description{The subset function can be used to return a single Move object from a MoveStack object.}

\usage{
  \S4method{[}{MoveStack,ANY,ANY}(x,i)
  \S4method{[[}{MoveStack,character,missing}(x,i)
}

\arguments{
  \item{x}{MoveStack object}
  \item{i}{numeric, number of position in the stack}
}

\details{
	The double square bracket method is used for sub setting a stack to a single move object according to the individual name.
}


\examples{
load(system.file("extdata", "move.RData", package="move"), .GlobalEnv)
st <- moveStack(list(leroy,ricky=ricky))
st[1]
st[['ricky']]
}

\author{Bart Kranstauber}
