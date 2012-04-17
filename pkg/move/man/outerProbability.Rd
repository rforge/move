\name{outerProbability}

\alias{outerProbability}
\alias{outerProbability,RasterLayer-method}

\docType{methods}

\title{Calculates the probabilities at the edges}

\description{The outerProbability method calculates the sum of the cells at the border of the raster.}

\usage{
  \S4method{outerProbability}{RasterLayer}(raster,border)
}

\arguments{
  \item{raster}{A raster with from class RasterLayer with calculated values}
  \item{border}{Describes the amount of columns at the border relative to the whole raster of which the summed probability should be calculated.}
}

\details{
  \code{border} is not necessary
  \code{raster} is not necessary
}

\author{Marco Smolla}
