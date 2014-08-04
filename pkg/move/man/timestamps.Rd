\name{timestamps}

\alias{timestamps}
\alias{timestamps<-}
\alias{timestamps,.MoveTrack-method}
\alias{timestamps,.unUsedRecords-method}
\alias{timestamps,.MoveTrackSingle-method}
\alias{timestamps<-,.MoveTrack-method}

\docType{methods}

\title{Extract the timestamps of a Move or MoveStack object}

\description{The timestamps method returns or sets the timestamps of a track from a Move or MovesStack object.}

\usage{
\S4method{timestamps}{.MoveTrackSingle}(this)
\S4method{timestamps}{.MoveTrack}(this)
\S4method{timestamps}{.MoveTrack}(this) <- value
}

\arguments{
  \item{this}{Move or MoveStack object}
  \item{value}{timestamps from class POSIXct}
}

\author{Marco Smolla}

\examples{
  load(system.file("extdata", "move.RData", package="move"), .GlobalEnv)
  stack <- moveStack(list(leroy,leroy))
  
  timestamps(leroy) #get the timestamps from a Move object
  timestamps(stack) #get the timestamps from a MoveStack object
  timestamps(leroy) <- timestamps(leroy)+60 #change the timestamps and set it for a Move object
  timestamps(stack) <- timestamps(stack)+60 #change the timestamps and set it for a MoveStack object
}
