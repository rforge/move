\name{Move-class}

\alias{Move-class}
\alias{.MoveTrack-class}
\alias{.MoveGeneral-class}
\alias{.MoveTrackSingle-class}
\alias{.MoveTrack}
\alias{.MoveTrackSingle}

\docType{class}

\title{
The Move class
}

\description{
The \code{Move} object contains at least time and coordinate information of an animal. It can contain further data that are individual to the animal, e.g. the sex or age. These data are stored in the idData data.frame. If the object was created with the Movebank browsing functions it does also contain the study name, licence and citation information.  
}

\section{Slots}{
  \describe{
    \item{idData}{Object of class \code{"data.frame"}: additional (one row) data;  }
    \item{dateCreation}{Object of class \code{"numeric"}: time stamp when the file was downloaded;  }
    \item{study}{Object of class \code{"character"}: name of the study;  }
    \item{citation}{Object of class \code{"character"}: how to cite the study;  }
    \item{license}{Object of class \code{"character"}: the license under which the data were published;  }
    \item{timesMissedFixes}{Object of class \code{"POSIXct"}: stores the timestamps of lines of the data set that were removed because they included NA locations }
    \item{bbox}{belongs to the SpatialPointsDataFrame}
    \item{coords}{coordinates of the track, belongs to the SpatialPointsDataFrame}
    \item{coords.nrs}{belongs to the SpatialPointsDataFrame}
    \item{data}{additional data of that object that is stored in the SpatialPointsDataFrame}
    \item{proj4string}{projection of the coordinates}
    \item{timestamps}{timestamps according to the coordinates}
  }
}

\section{Methods}{
  \describe{
    \item{\code{\link{move}}}{\code{signature(object = "Move")}: creates a Move object}
    \item{\code{\link{getMovebankData}}}{\code{signature(object = "character")}: creates a Move object by accessing Movebank}
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

\note{.MoveTrack and .MoveTrackSingle are also exported for other people to program against}
\author{Marco Smolla}

\keyword{classes}
