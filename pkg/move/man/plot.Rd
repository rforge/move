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
  \item{x}{A Move or a DBBMM object}
  \item{google}{Logical argument, that can be added to a Move object only. If TRUE, a map from googlemaps is downloaded.}
  \item{...}{Arguments to be passed to methods, such as graphical parameters, and the logical add argument (see par).}
}

\details{
  If \code{x} is a Move object a track is plotted with points and lines. The lines are created from the SpatialLines function, that extracts the coordinates from the Move object and transforms them into a SpatialLines object. The track can be added to a nother plot with the \code{add = TRUE}. \cr
  If \code{x} is a DBBMM object its raster object is plotted with the corresponding cell values.\cr
  The \code{google} argument works together with a Move object. If \code{TRUE} the GetMap funciton from the RgoogleMaps package generates a URL to download a map that fits with the track. After downloading the map, the track is plotted on top of the map.
}

\author{Marco Smolla}

\note{
  If you want to combine e.g. a track and a raster, or a track and a google map make sure that both objects have the same projection method. Use \code{projection()} to find it out and \code{spTransform()} to change the projection method of a Move object. 
}

\examples{

\dontrun{
#if x is a DBBMM object its raster object is plotted
plot(x)

#if x is a Move object, a track is plotted
plot(x)

#to add the track to the DBBMM raster plot
plot(x, add=TRUE)

#to plot a google map with overlayed track 
plot(x, google=TRUE)
}
}