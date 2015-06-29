\name{burstId}

\alias{burstId}
\alias{burstId,MoveBurst-method}
\alias{burstId,.MoveTrackSingleBurst-method}

\docType{methods}

\title{Returns burstId}

\description{Obtain a factor returning the ids of behavioral categorization per segment}

\usage{
  \S4method{burstId}{MoveBurst}(x)
}

\arguments{
  \item{x}{a MoveStack object}
}

\value{
	Returns a factor indicating the categorization
}

\author{Bart Kranstauber}

\examples{
  load(system.file("extdata", "move.RData", package="move"), .GlobalEnv)
  burstTrack <- burst(leroy,months(timestamps(leroy))[-1])
  burstId(burstTrack)
  
}
