\name{timestamps}

\alias{timestamps}
\alias{timestamps<-}
\alias{timestamps,.MoveTrack-method}
\alias{timestamps,.MoveTrackSingle-method}
\alias{timestamps<-,.MoveTrack-method}

\docType{methods}

\title{Extract the timestamps of a Move or MoveStack object}

\description{The timestmaps method returns or sets the timestamps of a track from a Move or MovesStack object.}

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
  load(system.file("extdata", "leroy.RData", package="move"), .GlobalEnv)
  
  timestamps(data) #get the timestamps from a Move object
  timestamps(data2) #get the timestamps from a MoveStack object
  timestamps(data) <- timestamps(data)+60 #change the timestamps and set it for a Move object
  timestamps(data2) <- timestamps(data2)+60 #change the timestamps and set it for a MoveStack object
}