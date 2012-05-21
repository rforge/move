\name{summary}

\alias{summary}
\alias{summary,DBBMM-method}
\alias{summary,Move-method}

\docType{methods}

\title{A summary of a DBBMM or Move object}

\description{Summarizes the information of the raster from a DBBMM or the data from a Move object}

\usage{
  \S4method{summary}{DBBMM}(object)
  \S4method{summary}{Move}(object)
}

\arguments{
  \item{object}{a DBBMM or Move object}
}

\details{
Returns the projection, extent, and maximum and minimum values of the raster stored within the DBBMM object.
}

\author{Marco Smolla}