\name{subset-method}

\alias{[,MoveStack,ANY,ANY-method}
\alias{[,.MoveTrack,ANY,ANY-method}
\alias{[,.MoveTrackSingleBurst,ANY,ANY-method}
\alias{[,.MoveTrackStack,ANY,ANY-method}
\alias{[,dBMvariance,ANY,ANY-method}
\alias{[,dBMvarianceStack,ANY,ANY-method}
\alias{[,dBMvarianceBurst,ANY,ANY-method}
\alias{[,.unUsedRecordsStack,ANY,ANY-method}
\alias{[,.unUsedRecords,ANY,ANY-method}
\alias{[[,MoveStack,character,missing-method}
\alias{[[,.MoveTrackStack,character,missing-method}
\alias{[[,.MoveTrackStack,logical,missing-method}
\alias{[[,.MoveTrackStack,numeric,missing-method}

\docType{methods}

\title{Returns a single object from a MoveStack}

\description{The subset function can be used to return a subset of a Move(Stack) object.}

\usage{
  \S4method{[}{MoveStack,ANY,ANY}(x,i)
  \S4method{[[}{MoveStack,character,missing}(x,i)
}

\arguments{
  \item{x}{MoveStack object}
  \item{i}{numeric, character or logical vector for individuals in a stack or a set of locations}
}

\details{
	The double square bracket method is used for sub setting a stack to a single move object according to the individual name or return a stack of multiple individuals. 
}


\examples{
load(system.file("extdata", "move.RData", package="move"))
st <- moveStack(list(leroy,ricky=ricky))
st[1]
st[['ricky']]
st[[2]]
st[[c(TRUE,FALSE)]]
}

\author{Bart Kranstauber}
