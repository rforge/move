\name{emd}

\alias{emd}
\alias{emd,RasterLayer,RasterLayer,numeric,logical,logical-method}
\alias{emd,DBBMM,DBBMM,numeric,logical,logical-method}


\docType{methods}

\title{Earth Movers Distance between DBBMM objects}

\description{Calculates the Earth Movers Distance between two rasters stored in DBBMM objects}

\usage{
  \S4method{emd}{RasterLayer,RasterLayer,numeric,logical,logical}(x, y, threshold, integer, greatcircle)
  \S4method{emd}{DBBMM,DBBMM,numeric,logical,logical}(x, y, threshold, integer, greatcircle)
}

\arguments{
  \item{x}{a DBBMM object}
  \item{y}{a DBBMM object}
  \item{threshold}{...}
  \item{integer}{...}
  \item{greatcircle}{...}
}

\details{
  ...
}

\author{Marco Smolla}

\note{
  ...
}

\examples{
  \dontrun{
    load(system.file("extdata", "leroy.RData", package="move"), .GlobalEnv)
  }
}