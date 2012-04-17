\name{SpatialLines}

\alias{SpatialLines}
\alias{SpatialLines,Move-method}

\docType{methods}

\title{Transform SpatialPoints to SpatialLines}

\description{Transform SpatialPoints to SpatialLines from a Move object}

\usage{
  \S4method{SpatialLines}{Move}(LinesList)
}

\arguments{
  \item{LinesList}{a Move object}
}

\details{
  The SpatialLines function transforms the SpatialPoints from the Move object to a SpatialLines object which can be used to plot a track or for further analysation. 
}

\author{Marco Smolla}