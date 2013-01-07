\name{'unUsedRecords<-'}

\alias{unUsedRecords<-}
\alias{unUsedRecords<-,.MoveTrackSingle,logical-method}
\alias{unUsedRecords<-,.MoveTrackStack,logical-method}

\title{Creates unused records data}

\description{
  A function that assigns locations as unused it could for example be used to remove locations test locations from a track.
}

\usage{
  \S4method{unUsedRecords}{.MoveTrackSingle,logical}(obj) <- value
  \S4method{unUsedRecords}{.MoveTrackStack,logical}(obj) <- value
}

\arguments{
  \item{obj}{a Move or MoveStack object}
  \item{value}{A logical vector of the same length as the number of locations}
}

\author{Marco Smolla}
