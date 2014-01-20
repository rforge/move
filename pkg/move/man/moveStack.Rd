\name{moveStack}

\alias{moveStack}
\alias{moveStack,list-method}
\alias{moveStack,character-method}

\docType{methods}

\title{Creating a MoveStack}

\description{Stacks a list of Move objects}

\usage{
  \S4method{moveStack}{list}(x)
}

\arguments{
  \item{x}{a list of Move objects}
}

\details{
  This function stacks single Move objects to a \link[=MoveStack-class]{MoveStack} object. 
}

\note{
All animal names are converted into 'good names' which means, that spaces are replaced with points and duplicated names get an individual number added. For example:\cr 
'Leroy, Leroy' -> adding number to duplicated names ->'Leroy, Leroy.1' \cr
'Ricky T' -> replacing spaces -> 'Ricky.T'
}

\author{Marco Smolla}

\examples{
load(system.file("extdata", "move.RData", package="move"), .GlobalEnv)

l <- list(ricky[200:270,], leroy[200:270,])
moveStack(l)

\dontshow{
data <- read.csv(system.file("extdata","ricky.csv.gz",package="move"))[100:199,]
ricky2 <- move(x=data$location.long, y=data$location.lat, time=as.POSIXct(data$timestamp,format="\%Y-\%m-\%d \%H:\%M:\%OS", tz="UTC"), proj=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"), data=data, animal=data$individual.local.identifier, sensor=data$sensor)
list <- list(leroy, ricky2)
stack <- moveStack(list)
}
}
