\name{move}

\alias{move}
\alias{move,character-method}
\alias{move,numeric-method}

\docType{methods}

\title{
Create a Move object
}

\description{
The move method creates a Move object from a Movebank file. Additionally the projection method must be set with the 'proj' argument.\cr
An alternative to this method is to import one's own data, and specify the location and timestamp columns. 
}

\usage{
\S4method{move}{character}(x, tz, proj)
\S4method{move}{numeric}(x, y, time, data, tz, proj)%proj = CRS("+proj=longlat +ellps=WGS84"), ...)
}

\arguments{
\item{x}{Full path to the file location, OR column indicator for non-Movebank data for the x coordinates; e.g. \code{data$x}}
\item{y}{column indicator for non-Movebank data for the y coordinates}
\item{time}{column indicator for non-Movebank data for the time stamps}
\item{data}{full parh of the location of the non-Movebank file}
\item{tz}{character indicating the timezone of the timestamps, e.g. \code{"GMT"}}
\item{proj}{projection method; requires a valid CRS (see \code{\link{CRS-class}}) object, CRS("+proj=longlat +ellps=WGS84") is default}
\item{...}{Additional arguments}  
}

\details{
  There are two ways to import data to make a Move object. Either the path to a file downloaded from \url{www.Movebank.org} is set for the x argument, or the file is first imported with the base \code{\link{read.csv}} function and then the columns with the x and y coordinates, and the timestamp, as well as the whole data.frame of the imported data are given to the \code{\link{move}} function. 
}

\note{
Because the SpatialPointsDataFrame function that creates the spatial data frame (\code{sdf}) of the \code{Move} object can not process NA location values, all rows with NA locations are omitted. A list of the omitted timestamps is stored in the \code{timesMissedFixes} argument of the \code{Move} object. \cr
If the data include double timestamps check your data for valitdity. You may want to consider a function to delete double timetamps, like: \cr \code{data <- data[which(!duplicated(data$timestamp)), ]}
}


\author{Marco Smolla }

\examples{
\dontrun{
## create a move object
data <- move(system.file("data/leroy_fisher_LaPoint.csv",package="move"))## create a move object
data <- move(system.file("data/leroy_fisher_LaPoint.csv",package="move"),proj=CRS("+proj=longlat"), tz="GMT")

## create a move object from non-Movebank data
file <- read.table(system.file("data/leroy_fisher_LaPoint.csv",package="move"), header=TRUE, sep=",", dec=".")
data <- move(x=file$location.long, y=file$location.lat, time=file$timestamp, data=file, proj=CRS("+proj=longlat"))
}
}