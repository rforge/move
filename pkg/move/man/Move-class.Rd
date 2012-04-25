\name{Move-class}

\alias{Move-class}

\docType{class}

\title{
The Move class
}

\description{
The Move object is created with the \code{\link{move}} function. It is the required object class for the \code{\link{brownian.bridge.dyn}} function which calculates the utilization density.
}

\section{Slots}{
  \describe{
    \item{sdf}{Object of class \code{"SpatialPointsDataFrame"}: track locations with corresponding time stamps;  }
    \item{animal}{Object of class \code{"character"}: animal ID or name;  }
    \item{species}{Object of class \code{"character"}: species name;  }
    \item{dateCreation}{Object of class \code{"numeric"}: time stamp when the file was downloaded;  }
    \item{study}{Object of class \code{"character"}: name of the study;  }
    \item{citation}{Object of class \code{"character"}: how to cite the study;  }
    \item{license}{Object of class \code{"character"}: the license under which the data were publicated;  }
    \item{timesMissedFixes}{Object of class \code{"POSIXct"}: stores the timestamps of lines of the data set that were removed because they included NA locations }
  }
}

\section{Methods}{
  \describe{
    \item{\code{\link{move}}}{\code{signature(object = "Move")}: creates a move object}
    \item{\code{\link{spTransform}}}{\code{signature(object = "Move")}: transforms coordinates to a different projection method}
    \item{\code{\link{show}}}{\code{signature(object = "Move")}: prints a summary of all data stored in the Move object}
    \item{\code{\link{as.data.frame}}}{\code{signature(object = "Move")}: extracts the spatial data frame}
    \item{\code{\link{coordinates}}}{\code{signature(object = "Move")}: extracts the coordinates only from the Move object}
    \item{\code{\link{proj4string}}}{\code{signature(object = "Move")}: extracts the projection method from the Move object}
    \item{\code{\link{time.lag}}}{\code{signature(object = "Move")}: calculates time lags between coordinates}
    \item{\code{\link{n.locs}}}{\code{signature(object = "Move")}: calculates number of locations}
    \item{\code{\link{plot}}}{\code{signature(object = "Move")}: plots the track of the animal}
  }
}

\author{Marco Smolla}

\keyword{classes}
