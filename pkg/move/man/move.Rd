\name{move}

\alias{move}
%\alias{move,character,ANY,ANY,ANY,ANY,ANY-method}
%\alias{move,character,ANY,ANY,ANY,ANY-method}
\alias{move,character,missing,missing,missing,missing-method}
\alias{move,numeric,numeric,POSIXct,data.frame,CRS-method}
\alias{move,numeric,numeric,POSIXct,missing,CRS-method}
%\alias{move,character-method}
%\alias{move,numeric,numeric,POSIXct,data.frame,CRS-method}

\docType{methods}

\title{
Create a Move object
}

\description{
The move method creates a Move object from a Movebank file. Additionally the projection method must be set with the 'proj' argument.\cr
An alternative to this method is to import one's own data, and specify the location and timestamp columns. 
}

\usage{
\S4method{move}{character,missing,missing,missing,missing}(x)
\S4method{move}{numeric,numeric,POSIXct,data.frame,CRS}(x, y, time, data, proj, sensor='unknown',animal='unnamed',...)
}

\arguments{
\item{x}{Full path to the file location, OR column indicator for non-Movebank data for the x coordinates; e.g. \code{data$x}}
\item{y}{column indicator for non-Movebank data for the y coordinates}
\item{time}{column indicator for non-Movebank data for the time stamps, with POSIXct conversion, i.e. \code{as.POSIXct(data$timestamp, format="\%Y-\%m-\%d \%H:\%M:\%S", tz="UTC")}}
\item{data}{Optional extra data associated with the relocation, if empty it is filled with the coordinates and timestamps}
\item{proj}{projection method for non-Movebank data; requires a valid CRS (see \code{\link{CRS-class}}) object, like CRS("+proj=longlat +ellps=WGS84")}
\item{sensor}{sensor name, either single character or a vector with length of the number of coordinates}
\item{animal}{animal ID or name, either single character or a vector with length of the number of coordinates}
\item{...}{Additional arguments}  
}

\details{
  There are two ways to import data to make a Move object. Either the path to a file downloaded from \url{www.movebank.org} is set for the x argument, or the file is first imported with the base \code{\link{read.csv}} function and then the columns with the x and y coordinates, and the timestamp, as well as the whole data.frame of the imported data are given to the \code{\link{move}} function. 
}

\note{
The imported data set is checked whether it is in a Movebank format. If this is not the case, you have to use the alternative import for non-Movebank data. \cr
Because the SpatialPointsDataFrame function that creates the spatial data frame (\code{sdf}) of the \code{Move} object can not process NA location values, all rows with NA locations are omitted. A list of the omitted timestamps is stored in the \code{timesMissedFixes} argument of the \code{Move} object. \cr
If the data include double timestamps check your data for valitdity. You may want to consider a function to delete double timetamps, like: \cr \code{data <- data[which(!duplicated(data$timestamp)), ]}
}


\author{Marco Smolla }

\examples{
## create a move object from a Movebank csv file
data <- move(system.file("extdata","leroy.csv",package="move"))

## create a move object from non-Movebank data
file <- read.table(system.file("extdata","leroy.csv",package="move"), header=TRUE, sep=",", dec=".")
data <- move(x=file$location.long, y=file$location.lat, time=as.POSIXct(file$timestamp, format="\%Y-\%m-\%d \%H:\%M:\%S", tz="UTC"), data=file, proj=CRS("+proj=longlat"), animal="Leroy")
\dontshow{move(x=file$location.long, y=file$location.lat, time=as.POSIXct(file$timestamp, format="\%Y-\%m-\%d \%H:\%M:\%S", tz="UTC"), data=file, proj=CRS("+proj=longlat"))
move(x=1:10,y=1:10,time=as.POSIXct(1:10, origin='1970-1-1'),proj=CRS('+proj=longlat'))
move(x=1:10,y=1:10,time=as.POSIXct(c(1:5,1:5), origin='1970-1-1'),proj=CRS('+proj=longlat'), animal=c(rep('a',5),rep('b',5)))}
}
