\name{summary}

\alias{summary}
\alias{summary,.UD-method}
\alias{summary,.UDStack-method}
\alias{summary,.MoveTrackSingle-method}
\alias{summary,.MoveTrackStack-method}

\docType{methods}

\title{A summary of a DBBMM or DBBMMStack, Move or MoveStack object}

\description{Summarizes the information of the raster from a DBBMM or DBBMMStack, or the data from a Move or MoveStack object}

\usage{
  \S4method{summary}{.UD}(object)
  \S4method{summary}{.UDStack}(object)
  \S4method{summary}{.MoveTrackSingle}(object)
  \S4method{summary}{.MoveTrackStack}(object)
}

\arguments{
  \item{object}{a DBBMM/DBBMMStack, Move/MoveStack object}
}

\details{
Returns the projection, extent, and maximum and minimum values of the raster stored within the DBBMM or DBBMMStack object. If the object is a Move or MoveStack object the functions distance, time, speed, and angle is called and a list with the summarized values is returned. 
}

\author{Marco Smolla}

\examples{
  load(system.file("extdata", "move.RData", package="move"), .GlobalEnv)
  
  summary(leroy) # summary of angle measures of a Move object
  summary(stack) # summary of angle measures of a MoveStack object
  summary(leroydbbmm) # summary of a DBBMM object
  summary(dbbmmstack) # summary of a DBBMMStack object
}