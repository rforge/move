\name{proj4string}

\alias{proj4string,Move-method}
\alias{proj4string,DBBMM-method}

\docType{methods}

\title{Get a proj4string}

\description{Get the proj4string of the raster from a Move or DBBMM object.}

\usage{
  \S4method{proj4string}{Move}(obj)
  \S4method{proj4string}{DBBMM}(obj)
}

\arguments{
  \item{obj}{a Move or a DBBMM object}
}

\details{
  proj4strings are done by with the PROJ.4 library exposed by rgdal
}

\value{
  a CRS like character
}

\author{Marco Smolla}
