\name{burst}

\alias{burst}
\alias{burst,Move,character-method}
\alias{burst,Move,factor-method}
\alias{burst,Move,numeric-method}
\alias{burst,.MoveTrackSingleBurst,missing-method}

\docType{methods}

\title{Bursting a track}

\description{Bursting a track by specified variable}

\usage{
  \S4method{burst}{Move,character}(x, f, ...)
}

\arguments{
  \item{x}{a Move object}
  \item{f}{a character, factor, or numeric that indicates how to burst the coordinates of a Move object. It must be one shorter than the number of locations, because there are always one less segments of a track than coordinates}
  \item{...}{not used}
}

\details{
The burst function bursts (divides) a track in segments that are specified by the burstIDs (e.g. behavioral annotations). It allows to investigate different parts of a track according to supplied variables like day and night, movement and rest, and so on.  
}

\author{Marco Smolla}

\examples{
test <- move(system.file("extdata","leroy.csv.gz", package="move"))
behav <- c(rep(1:4,each=200), rep(5, 118))
testb <- burst(x=test, f=behav)
head(testb)
}
