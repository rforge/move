\name{moveStack}

\alias{moveStack}
\alias{moveStack,list-method}
\alias{moveStack,character-method}

\docType{methods}

\title{Creating a MoveStack}

\description{Either stacks a list of Move objects or creates a MoveStack from a Movebank csv file}

\usage{
  \S4method{moveStack}{list}(x)
  \S4method{moveStack}{character}(x, proj)
}

\arguments{
  \item{x}{a list of Move objects or the path to a csv file as character}
  \item{proj}{the projection of the coordinates}
}

\details{
  This function stacks single Move objects to a \link[=MoveStack-class]{MoveStack} object, or creates a MoveStack from a Movebank file with several individuals. 
}

\author{Marco Smolla}

\examples{
\dontshow{
#dataOne<-move(x=1:10,y=1:10,time=as.POSIXct(1:10, origin='1970-1-1'),proj=CRS('+proj=longlat'))
#dataTwo<-move(x=1:10,y=1:10,time=as.POSIXct(1:10, origin='1970-1-1'),proj=CRS('+proj=longlat'))
#moveStack(list(dataOne, dataTwo)) ##Number of sensors observations does not match the number of coordinates
}
}
