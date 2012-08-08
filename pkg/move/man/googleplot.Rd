\name{googleplot}

\alias{googleplot}
\alias{googleplot,.MoveTrack-method}
\alias{googleplot,Move-method}
\alias{googleplot,MoveStack-method}

\docType{methods}

\title{Plot tracks on a Google map}

\description{Function for plotting a track from a Move/Movestack object}

\usage{
  \S4method{googleplot}{Move}(obj,...)
  \S4method{googleplot}{MoveStack}(obj,...)
}

\arguments{
  \item{obj}{Move or MoveStack object}
  \item{...}{arguments to be passed to methods, such as graphical parameters, and the logical add argument (see \code{\link{par}})}
}

\details{
Use the googlemap function to get an impression of where the tracks are situated on a real map. You can both use single tracks from a Move object and multiple tracks from a MoveStack object (note: multiple tracks will all have the same color). 
Add \code{maptype} as an argument to change the type of map google returnes (e.g. "terrain", "satellite", "roadmap"). You can also use the standard \code{par} parameters to change the look of the track (\code{color} or \code{lwd} can for example be used to change the color and the line width of the track).
}

\author{Marco Smolla}


\examples{
load(system.file("extdata", "leroy.RData", package="move"), .GlobalEnv)
googleplot(data, col=5, lwd=2, maptype="terrain")
\dontshow{
googleplot(data2, lwd=2, col=5, maptype="satellite")
}
}
