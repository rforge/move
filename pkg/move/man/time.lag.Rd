\name{time.lag}

\alias{time.lag}
\alias{time.lag,.MoveTrackSingle-method}
\alias{time.lag,.MoveTrackStack-method}

\docType{methods}

\title{Calculates the time lags between the coordinates}

\description{The time.lag function calculates the time lags between locations.}

\usage{
  \S4method{time.lag}{.MoveTrackSingle}(x,...)
  \S4method{time.lag}{.MoveTrackStack}(x,units, ...)
}

\arguments{
  \item{x}{a Move or MoveStack object}
  \item{units}{The units used for the conversion, they need to be specified for a stack to ensure similair units between individuals}
  \item{...}{further arguments}
}

\details{This function is now replaced by timeLag}




\author{Marco Smolla}

