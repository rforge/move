\name{plot}

\alias{plot}
\alias{plot,.MoveTrackSingle,missing-method}
\alias{plot,MoveStack,missing-method}

\docType{methods}

\title{Plotting a track or a raster}

\description{Function for plotting a recorded track from a Move object or the probability values from a DBBMM object}

\usage{
  \S4method{plot}{.MoveTrackSingle,missing}(x, y,...)
  \S4method{plot}{MoveStack,missing}(x, y, col=NA, ...)
}

\arguments{
  \item{x}{Move or a DBBMM object}
  \item{y}{keep empty}
%  \item{google}{logical argument for a Move object. If TRUE, a map from googlemaps is downloaded and the track is plotted on top}
%  \item{maptype}{defines the type of map to construct. There are several possible maptype values, including satellite, terrain, hybrid, and mobile.}
  \item{col}{specifies color. For Movestacks default is NA (R colors are used). It can also be a vector wich is as long as the number of tracks or coordinates}
  \item{...}{arguments to be passed to methods, such as graphical parameters, and the logical add argument (see \code{\link{par}})}
}

\details{
  If \code{x} is a Move object a track is plotted with points and lines. The track can be added to another plot with the \code{add = TRUE}. \cr
  If \code{x} is a DBBMM object its raster object is plotted with the corresponding cell values. Unlike the \code{\link{image}} function, the cell size ratio keeps the same when the plot window is resized. \cr
  %The \code{google} argument works together with a Move object. If \code{TRUE} the GetMap function from the RgoogleMaps package generates a URL to download a map that fits with the track. After downloading the map, the track is plotted on top of the map.
}

\author{Marco Smolla}

\note{
  If you want to combine e.g. a track and a raster, or a track and a google map, make sure that both objects have the same projection method. Use \code{\link{proj4string}} to find it out and \code{\link{spTransform}} to change the projection method of a Move object. 
}

\examples{
load(system.file("extdata", "leroy.RData", package="move"), .GlobalEnv)

## plot a Move object
plot(data)

#to plot a google map with overlayed track 
#plot(data, google=TRUE)

## add a track from a Move object to a plot
lines(data, add=TRUE)

## plot the raster of a DBBMM object
plot(dbbmm)

## plot the raster of a DBBMMStack object
plot(dbbmm2)
}
