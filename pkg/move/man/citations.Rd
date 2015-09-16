\name{citations}

\alias{citations}
\alias{citations<-}
\alias{citations,.MoveGeneral-method}
\alias{citations<-,.MoveGeneral-method}

\docType{methods}

\title{Extract the citation of a Move or MoveStack object}

\description{The citations method returns or sets the citation of a track from a Move or MovesStack object.}

\usage{
  \S4method{citations}{.MoveGeneral}(obj)
  \S4method{citations}{.MoveGeneral}(obj) <- value
}

\arguments{
  \item{obj}{Move or MoveStack object}
  \item{value}{citation from class character}
}

\author{Marco Smolla}

\examples{
  data(leroy)
  citations(leroy) #get the citation from a Move object
  citations(leroy) <- "No paper available" #change the citation and set it for a Move object
  data(fishers)
  citations(fishers) #get the citation from a MoveStack object
  citations(fishers) <- "Nothing to cite" #change the citation and set it for a MoveStack object
}