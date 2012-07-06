\name{plotBursts}

\alias{plotBursts}
\alias{plotBursts,list-method}
\alias{plotBursts,SpatialPointsDataFrame-method}

\docType{methods}

\title{Plotting the centroids of a track}

\description{Plotting centroids ...}

\usage{
  \S4method{plotBursts}{list}(object, add=TRUE, ...)
  \S4method{plotBursts}{SpatialPointsDataFrame}(object, add, ...)
}

\arguments{
  \item{object}{a SpatialPointsDataFrame or a list of these that include coordinates and color, and size of the centroid indicators}
  \item{add}{logical, if FALSE a new plot is generated. Default value is TRUE}
  \item{...}{additional plot attributes}
}

\details{
The SpatialPointsDataFrame that should be used here is generated by \code{\link{burstTrack}}.
}

\author{Marco Smolla}

\note{
...
}

\examples{
  \dontrun{
we need an anotated file here
  }
}