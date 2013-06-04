\name{turnAngleGc}

\alias{turnAngleGc}
\alias{turnAngleGc,.MoveTrackSingle-method}

\docType{methods}

\title{Calculates turning angles on great circle tracks}

\description{This function returns the turn angels of a great circle track.}

\usage{
  \S4method{turnAngleGc}{.MoveTrackSingle}(x)
}

\arguments{
  \item{x}{Move object, in long lat projection}
}

\details{
	On great circle tracks the bearing of arrival on a point is not the same as witch the previous point was left. This function returns the difference between these bearings between -180 and 180. The bearings are calculated using the functions bearing and finalBearing of the geosphere package.
}

\author{Bart Kranstauber}

\value{
	 The function returns the turning angles in degrees. 
}

\examples{
  load(system.file("extdata", "move.RData", package="move"), .GlobalEnv)  
  turnAngleGc(leroy)
}
