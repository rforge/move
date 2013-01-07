\name{'unUsedRecords<-'}

\alias{unUsedRecords<-}
\alias{unUsedRecords<-,.MoveTrackSingle,logical-method}
\alias{unUsedRecords<-,.MoveTrackStack,logical-method}

\title{Creates unused records data}

\description{
  A function that creates unused records data. It can be used by advanced programmers to program against.
}

\usage{
  \S4method{unUsedRecords}{.MoveTrackSingle,logical}(obj) <- value
  \S4method{unUsedRecords}{.MoveTrackStack,logical}(obj) <- value
}

\arguments{
  \item{obj}{a Move or MoveStack object}
  \item{value}{according values}
}

\author{Marco Smolla}
