\name{angle}

\alias{angle}
\alias{angle,.MoveTrackSingle-method}
\alias{angle,.MoveTrackStack-method}

\docType{methods}

\title{angle information from a track or track stack}

\description{This function returns a summary about angle related measurments of a track or track stack}

\usage{
  \S4method{angle}{.MoveTrackSingle}(x,values=FALSE)
  \S4method{angle}{.MoveTrackStack}(x,values=FALSE)
}

\arguments{
  \item{x}{Move or MoveStack object}
  \item{values}{if TRUE the angle values are returned, default is FALSE}
}

\author{Marco Smolla}

\examples{
  load(system.file("extdata", "leroy.RData", package="move"), .GlobalEnv)
  
  angle(data) # summary of angle measures of a Move object
  angle(data2) # summary of angle measures of a MoveStack object
  angle(data, values=TRUE) #angles from a Move object 
  angle(testtest2, values=TRUE) #angles from a MoveStack object
}