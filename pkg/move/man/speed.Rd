\name{speed}

\alias{speed}
\alias{speed,.MoveTrackSingle-method}
\alias{speed,.MoveTrackStack-method}

\docType{methods}

\title{speed information from a track or track stack}

\description{This function returns a summary about speed related measurments of a track or track stack}

\usage{
  \S4method{speed}{.MoveTrackSingle}(x,values=FALSE)
  \S4method{speed}{.MoveTrackStack}(x,values=FALSE)
}

\arguments{
  \item{x}{Move or MoveStack object}
  \item{values}{if TRUE the speed values are returned, default is FALSE}
}

\author{Marco Smolla}

\examples{
  load(system.file("extdata", "leroy.RData", package="move"), .GlobalEnv)
  
  speed(data) # summary of speed measures of a Move object
  speed(data2) # summary of speed measures of a MoveStack object
  speed(data, values=TRUE) #speeds from a Move object 
  speed(testtest2, values=TRUE) #speeds from a MoveStack object
}