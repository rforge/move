\name{emd}

\alias{emd}
\alias{emd,.UD,.UD,numeric,logical,logical-method}


\docType{methods}

\title{Earth Movers Distance between DBBMM objects}

\description{Calculates the Earth Movers Distance between two rasters stored in DBBMM objects}

\usage{
  \S4method{emd}{.UD,.UD,numeric,logical,logical}(x, y, threshold, integer, greatcircle)
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
    \dontshow{
      .C('emdR', as.integer(4) ,as.integer(4),as.double(1:4/4),as.double(1:4/4), as.double(1:4/4), as.double(1:4/4), as.double(1:4/4), as.double(4:1/4), as.double(1.3), as.double(100.3), as.integer(0))
    }
  }
}