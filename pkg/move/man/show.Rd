\name{show}

\alias{show}
\alias{show,Move-method}
\alias{show,MoveStack-method}
\alias{show,MoveBurst-method}
\alias{show,dBMvariance-method}
\alias{show,dBMvarianceTmp-method}
\alias{show,.MoveTrack-method}
\alias{show,.MoveGeneral-method}
\alias{show,.MoveTrackStack-method}
\alias{show,.MoveTrackSingle-method}
\alias{show,.MoveTrackSingleBurst-method}

\docType{methods}

\title{Show a Move object}

\description{Displays a summary of a Move object.}

\usage{
  \S4method{show}{Move}(object)
  \S4method{show}{MoveStack}(object)
}

\arguments{
  \item{object}{a Move or MoveStack object}
  %\item{...}{arguments handed over; here to the \fct{cat} function; use fill to specify whether line breaks (logical) should be inserted or how long a line shoud maximally be (numerical)}
}

\details{
  The show function displays a summary of a Move object. This includes:
    \itemize{
    \item animal ID
    \item species name
    \item study name
    \item number of track points
    \item receiver type
    \item projection method
    \item date of file creation
    \item the first three lines of the spatial data frame
    \item study citation
    \item data license
    \item number of omitted locations due to NAs in the dataset
    }
    If the imported data are not from the Movebank database Animal, Species, nPoints, Receiver, and Study are not shown. 
}

\author{Marco Smolla}
