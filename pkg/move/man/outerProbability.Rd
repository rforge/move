\name{outerProbability}

\alias{outerProbability}
\alias{outerProbability,RasterLayer-method}

\docType{methods}

\title{Calculates the probabilities at the edges of a raster}

\description{The outerProbability method calculates the summed probabilitiy of the cells at the border of a raster}

\usage{
  \S4method{outerProbability}{RasterLayer}(raster,border)
}

\arguments{
  \item{raster}{a RasterLayer object that has values for the raster cells}
  \item{border}{numeric from 0 to 1; ratio of the number of columns at the border relative to the whole raster from which the probabilities should be summed up}
}

\examples{
\dontrun{
## create a move object, transform it, and create a DBBMM object
data <- move(system.file("data/leroy.csv",package="move"),proj=CRS("+proj=longlat"))
data2 <- spTransform(data, center=TRUE)
p <- brownian.bridge.dyn(object=data2, location.error=23.5, dimSize=45, ext=.2)

## calculate the probabilities of 20\% of the raster at the border
outerProbability(raster(p), border=.2)
[1] 0.06753346
}
}

\author{Marco Smolla}
