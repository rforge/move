\name{burstTrack}

\alias{burstTrack}
\alias{burstTrack,Move,integer-method}

\docType{methods}

\title{Bursting a track}

\description{Bursting a track by specified variable}

\usage{
  \S4method{burstTrack}{Move,integer}(object, by, breaks=5, sizeFUN="relTime")
}

\arguments{
  \item{object}{a Move object}
  \item{by}{column indicator like \code{data$behavior} that is used to burst the track}
  \item{breaks}{defines the number of steps by which the relative values, e.g. relative time, is divided. Default value is 5.}
  \item{sizeFUN}{specify a function to calculate the sizes of the centroids like \code{function(ll) lapply(lapply(ll, length), FUN= function(x) (x/nrow(coordinates(object))))} DOES NOT WORK AT THE MOMENT! Default is relative time of the burst segemnt compared to the whole track}
}

\details{
  sizeFUN does not work at the moment
  The burstTrack function bursts (divides) a track in segments that are specified by the by attribute. If a colum with behavioral classfication is annotated to the coordinates of the Move object, the track can be bursted by that column. For every segment an indicator (a circle) is set in the middle of the segment. The size of this indicator depends on the sizeFUN and is by default defined by the relative time compared to the whole duration. 
}

\author{Marco Smolla}

\examples{
##here we need a annotated csv file
}