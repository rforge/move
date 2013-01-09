\name{raster2contour}

\alias{raster2contour}
\alias{raster2contour,.UD-method}
\alias{raster2contour,.UDStack-method}

\docType{methods}

\title{Convert raster to contour lines}

\description{The function converts a raster object to a SpatialLinesDataFrame. }

\usage{
  \S4method{raster2contour}{.UD}(x, ...)
  \S4method{raster2contour}{.UDStack}(x, ...)
}

\arguments{
  \item{x}{a DBBMM or DBBMMStack object, that includesa raster object}
  \item{...}{additional arguments that are passed on from other functions}
}

\details{
  The raster2contour function creates a \link[=SpatialLinesDataFrame-class]{SpatialLinesDataFrame} from a given raster or \link[=DBBMM-class]{DBBMM} object. This allows to re-project the contours to different projections. 
}

\author{Marco Smolla}

\examples{
  load(system.file("extdata", "move.RData", package="move"), .GlobalEnv)
  raster2contour(leroydbbmm)
  raster2contour(dbbmmstack)
}