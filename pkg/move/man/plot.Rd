\name{plot}

\alias{plot,Move,ANY-method}
\alias{plot,DBBMM,ANY-method}

\docType{methods}

\title{Plotting a track or a raster}

\description{Function for plotting a recorded track from a Move object or the probabilitiy values from a DBBMM object}

\usage{
  \S4method{plot}{Move,ANY}(x,google,...)
  \S4method{plot}{DBBMM,ANY}(x)
}

\arguments{
  \item{x}{Move or a DBBMM object}
  \item{google}{logical argument for a Move object. If TRUE, a map from googlemaps is downloaded and the track is plotted on top}
  \item{...}{arguments to be passed to methods, such as graphical parameters, and the logical add argument (see \code{\link{par}})}
}

\details{
  If \code{x} is a Move object a track is plotted with points and lines. The lines are created from the SpatialLines function, that extracts the coordinates from the Move object and transforms them into a SpatialLines object. The track can be added to a nother plot with the \code{add = TRUE}. \cr
  If \code{x} is a DBBMM object its raster object is plotted with the corresponding cell values. Compared to the \code{\link{image}} function, the cell size ratio keeps the same, when the plot window is resized. \cr
  The \code{google} argument works together with a Move object. If \code{TRUE} the GetMap funciton from the RgoogleMaps package generates a URL to download a map that fits with the track. After downloading the map, the track is plotted on top of the map.
}

\author{Marco Smolla}

\note{
  If you want to combine e.g. a track and a raster, or a track and a google map make sure that both objects have the same projection method. Use \code{\link{proj4string}} to find it out and \code{\link{spTransform}} to change the projection method of a Move object. 
}

\examples{
\dontrun{
## create a move object
data <- move(system.file("data/leroy_fisher_LaPoint.csv",package="move"),proj=CRS("+proj=longlat"))

## plot a Move object
plot(data)

#to plot a google map with overlayed track 
plot(data, google=TRUE)

## add a track from a Move object to a plot
plot(data, add=TRUE)

## transform the Move object and create a DBBMM object
data2 <- spTransform(data, center=TRUE)
p <- brownian.bridge.dyn(object=data2, location.error=23.5, dimSize=45, ext=.2)

## plot the raster of a DBBMM object
plot(p)
}
}