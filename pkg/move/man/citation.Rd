\name{citation}

\alias{citation}
\alias{citation<-}
\alias{citation,.MoveGeneral-method}
\alias{citation<-,.MoveGeneral-method}

\docType{methods}

\title{Extract the citation of a Move or MoveStack object}

\description{The citation method returns or sets the citation of a track from a Move or MovesStack object.}

\usage{
  \S4method{citation}{.MoveGeneral}(obj)
  \S4method{citation}{.MoveGeneral}(obj) <- value
}

\arguments{
  \item{obj}{Move or MoveStack object}
  \item{value}{citation from class character}
}

\author{Marco Smolla}

\examples{
  load(system.file("extdata", "leroy.RData", package="move"), .GlobalEnv)
  
  citation(data) #get the citation from a Move object
  citation(data2) #get the citation from a MoveStack object
  citation(data) <- "No paper available" #change the citation and set it for a Move object
  citation(data2) <- "Nothing to cite" #change the citation and set it for a MoveStack object
}