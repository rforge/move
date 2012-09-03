\name{moveStack}

\alias{moveStack}
\alias{moveStack,list-method}
\alias{moveStack,character-method}

\docType{methods}

\title{Creating a MoveStack}

\description{Stacks a list of Move objects}

\usage{
  \S4method{moveStack}{list}(x)
  %\S4method{moveStack}{character}(x, proj)
}

\arguments{
  \item{x}{a list of Move objects}
  %\item{proj}{the projection of the coordinates}
}

\details{
  This function stacks single Move objects to a \link[=MoveStack-class]{MoveStack} object. %, or creates a MoveStack from a Movebank file with several individuals. 
}

\note{
All animal names are converted into 'good names' which means, that the names have no space and are unique. 
'Leroy, Leroy' becomes 'Leroy, Leroy.1' 
'Ricky T' becomes 'Ricky.T'
}

\author{Marco Smolla}

\examples{
\dontshow{
load(system.file("extdata", "move.RData", package="move"), .GlobalEnv)
moveStack(list(ricky, leroy))
}
}
