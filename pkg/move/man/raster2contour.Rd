\name{raster2contour}

\alias{raster2contour}
\alias{raster2contour,RasterLayer-method}

\docType{methods}

\title{Convert raster to contour lines}

\description{The funciton converts a raster object to a SpatialLinesDataFrame. }

\usage{
  \S4method{raster2contour}{RasterLayer}(x, ...)
}

\arguments{
  \item{x}{a RasterLayer or a DBBMM object}
  \item{...}{additional arguments that are passed on from other functions}
}

\details{
  The raster2contour function creates a \link[=SpatialLinesDataFrame-class]{SpatialLinesDataFrame} from a given raster or \link[=DBBMM-class]{DBBMM} object. This allows to reproject the contours to different projections. 
}

\author{Marco Smolla}

\examples{
  load(system.file("extdata", "move.RData", package="move"), .GlobalEnv)
  raster2contour(leroydbbmm)
}