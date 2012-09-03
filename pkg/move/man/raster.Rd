\name{raster}

\alias{raster}
\alias{raster,DBBMM-method}
\alias{raster,DBBMMStack-method}

\docType{methods}

\title{Extract raster from DBBMM}

\description{Extrachts the RasterLayer from a DBBMM and DBBMMStack object.}

\usage{
  \S4method{raster}{DBBMM}(x)
  \S4method{raster}{DBBMMStack}(x)
}

\arguments{
  \item{x}{a DBBMM or DBBMMStack object}
}

\details{
  The raster function extraxcts the raster object from a DBBMM or DBBMMStack object.
}

\value{
  An object from class RasterLayer is returned.
}

\author{Marco Smolla}

\examples{
load(system.file("extdata", "move.RData", package="move"), .GlobalEnv)

raster(rickydbbmm) #returns the raster of a DBBMM object
raster(dbbmmstack) # returns the raster of a DBBMMStack object

plot(raster2contour(rickydbbmm, levels=c(.5,.9)), col=c(5,4)) # plot the raster lines of a DBBMM
}