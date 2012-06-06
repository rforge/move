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
load(system.file("extdata", "leroy.RData", package="move"), .GlobalEnv)

## calculate the probabilities of 20\% of the raster at 
## the border from the DBBMM object dbbmm
outerProbability(dbbmm, border=.2)
"[1] 0.06753346"
}

\author{Marco Smolla}
