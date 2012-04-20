\name{move}

\alias{move}
\alias{move,character-method}
\alias{move,numeric-method}

\docType{methods}

\title{
Create a Move object
}

\description{
The move method creates a Move object from a movebank file. Additional the projection method must be set with the 'proj' argument.\cr
Alternative to this method is to import own data, and specify the location and timestamp cloumns. 
}

\usage{
\S4method{move}{character}(x, proj)
\S4method{move}{numeric}(x, y, time, data, proj)%proj = CRS("+proj=longlat +ellps=WGS84"), ...)
}

\arguments{
\item{x}{Full path to the file location, OR column indicator for non-movebank data for the x coordinates; e.g. \code{data$x}}
\item{y}{column indicator for non-movebank data for the y coordinates}
\item{time}{column indicator for non-movebank data for the time stamps}
\item{data}{full parh of the location of the non-movebank file}
\item{proj}{projection method; requires a valid CRS object}
\item{...}{Additional arguments}  
}

\details{
  There are two ways to import data to make a Move object. Either the path to a file downloaded from \url{www.movebank.org} is set for the x argument, or the file is first imported with the base \code{\link{read.csv}} funciton and then the columns with the x and y coordinates, and the timestamp, as well as the whole data.frame of the imported data is given to the \code{move} function. 
}


\author{Marco Smolla }

\examples{
## create a move object
data <- move(system.file("data/leroy_fisher_LaPoint.csv",package="move"))## create a move object
data <- move(system.file("data/leroy_fisher_LaPoint.csv",package="move"),proj=CRS("+proj=longlat"))

## create a move object from non-movebank data
file <- read.table(system.file("data/leroy_fisher_LaPoint.csv",package="move"), header=TRUE, sep=",", dec=".")
data <- move(x=file$location.long, y=file$location.lat, time=file$timestamp, data=file, proj=CRS("+proj=longlat"))
}