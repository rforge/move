\name{Move-class}

\alias{Move-class}

\docType{class}

\title{
The Move class
}

\description{
The Move object is created with the move function (see move) from a valid data file and necessary for the calculation of the utilization density with the \code{brownian.bridge.dyn} function. 

}

\section{Slots}{
  \describe{
    \item{sdf}{Object of class \code{"SpatialPointsDataFrame"}: track locations with corresponding time stamps;  }
    \item{animal}{Object of class \code{"character"}: animal ID or name;  }
    \item{species}{Object of class \code{"character"}: species name;  }
    \item{dateCreation}{Object of class \code{"numeric"}: time stamps when the file was downloaded;  }
    \item{study}{Object of class \code{"character"}: name of the study;  }
    \item{citation}{Object of class \code{"character"}: how to cite the study;  }
    \item{license}{Object of class \code{"character"}: under which licens were the data publicated;  }
    \item{timesMissedFixes}{Object of class \code{"POSIXct"}: stores the timestamps of lines of the data set that were removed because they included NA locations }
  }
}

\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "Move")}: print a summary of all data stored in the Move object }
  }
}


\arguments{
\item{projargs}{A character string of projection arguments; the arguments must be entered exactly as in the PROJ.4 documentation; if the projection is unknown, use \code{as.character(NA)}, it may be missing or an empty string of zero length and will then set to the missing value.}
}

\author{Marco Smolla}

\keyword{classes}
