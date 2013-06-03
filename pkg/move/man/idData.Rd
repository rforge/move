\name{idData}

\alias{idData}
\alias{idData<-}
\alias{idData<-,.MoveTrack,ANY,ANY,ANY-method}
\alias{idData<-,.MoveTrack,missing,missing,data.frame-method}
\alias{idData,.MoveTrack-method}

\docType{methods}

\title{Functions for dealing with the idData}

\description{This function returns or replaces the idData which is the data per individual.}

\usage{
  \S4method{idData}{.MoveTrack,missing,missing,data.frame}(x,i,j) <- value
  \S4method{idData}{.MoveTrack,ANY,ANY,ANY}(x,i,j) <- value
  \S4method{idData}{.MoveTrack}(x,i,j,...)
}

\arguments{
  \item{x}{Move, MoveBurst or MoveStack object}
  \item{i}{Selection of the rows}
  \item{j}{Selection for the columns}
  \item{value}{Replacement values for the selected idData}
  \item{...}{Other arguments to the data frame subseting such as drop=F}
}

\author{Bart Kranstauber}

\value{
	Either the idData data frame or the modified move object
}

\examples{
  load(system.file("extdata", "move.RData", package="move"), .GlobalEnv)
	idData(leroy)
}
