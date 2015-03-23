\name{move}

\alias{move}
%\alias{move,character,ANY,ANY,ANY,ANY,ANY-method}
%\alias{move,character,ANY,ANY,ANY,ANY-method}
\alias{move,character,missing,missing,missing,missing-method}
\alias{move,connection,missing,missing,missing,missing-method}
\alias{move,numeric,numeric,POSIXct,data.frame,ANY-method}
\alias{move,numeric,numeric,POSIXct,missing,ANY-method}
%\alias{move,character-method}
%\alias{move,numeric,numeric,POSIXct,data.frame,CRS-method}

\docType{methods}

\title{
Create a Move object
}

\description{
The move method creates Move or MoveStack object from Movebank or other (compressed) csv files, also zip files from the environmental annotation tool can be loaded. If you use your own data you need to set the projection method with the 'proj' argument and specify in which column of your data the function finds locations and timestamps. 
}

\usage{
\S4method{move}{connection,missing,missing,missing,missing}(x, removeDuplicatedTimestamps=F, ...)
\S4method{move}{numeric,numeric,POSIXct,data.frame,ANY}(x, y, time, data, proj, sensor='unknown',animal='unnamed',...)
}

\arguments{
\item{x}{Full path to the file location, OR a vector with x coordinates if non-Movebank data are provided (e.g. \code{data$x})}
\item{y}{vector of y coordinates if non-Movebank data are provided}
\item{time}{column indicator for non-Movebank data for the time stamps, with POSIXct conversion, i.e. \code{as.POSIXct(data$timestamp, format="\%Y-\%m-\%d \%H:\%M:\%S", tz="UTC")}}
\item{data}{Optional extra data associated with the relocation, if empty it is filled with the coordinates and timestamps}
\item{proj}{projection method for non-Movebank data; requires a valid CRS (see \code{\link{CRS-class}}) object, like CRS("+proj=longlat +ellps=WGS84"); default is NA}
\item{sensor}{sensor name, either single character or a vector with length of the number of coordinates}
\item{animal}{animal ID or name, either single character or a vector with length of the number of coordinates}
\item{removeDuplicatedTimestamps}{It his possible to add the argument removeDuplicatedTimestamps and set it to true which allows you delete the duplicated timestamps, it is strongly advised not to use this option because there is no control over which records are removed. Its better to edit the records in movebank.}
\item{...}{Additional arguments}  
}

\details{
The easiest way to import data is to download the study you are interested in from \url{www.movebank.org}. Set the file path as the x argument of the move function. The function detects whether there are single or multiple individuals in this file and automatically creates either a Move or MoveStack object. 
Another way is to read in your data using \code{\link{read.csv}}. Then the columns with the x and y coordinates, and the timestamp, as well as the whole data.frame of the imported data are given to the \code{\link{move}} function. Again the function detects whether to return a Move or a MoveStack object
}

\note{
The imported data set is checked whether it is in a Movebank format. If this is not the case, you have to use the alternative import for non-Movebank data (see above). 
Because the SpatialPointsDataFrame function that creates the spatial data frame (\code{sdf}) of the \code{Move} object can not process NA location values, all rows with NA locations are omitted. A list of the omitted timestamps is stored in the \code{timesMissedFixes} slot of the \code{Move} object. \cr
If the data include double timestamps check your data for validity. You may want to consider a function to delete double timestamps, like: \cr \code{data <- data[which(!duplicated(data$timestamp)), ]}

Due to convention all names are turned into 'good names' which means, without spaces ('Ricky T' becomes 'Ricky.T').

}


\author{Marco Smolla}

\examples{
## create a move object from a Movebank csv file
filePath<-system.file("extdata","leroy.csv.gz",package="move")
data <- move(filePath)

## create a move object from non-Movebank data
file <- read.table(filePath, 
		   header=TRUE, sep=",", dec=".")
data <- move(x=file$location.long, y=file$location.lat, 
	     time=as.POSIXct(file$timestamp, 
			     format="\%Y-\%m-\%d \%H:\%M:\%S", tz="UTC"), 
	     data=file, proj=CRS("+proj=longlat +ellps=WGS84"), animal="Leroy")
\dontshow{
move(x=file$location.long, y=file$location.lat, time=as.POSIXct(file$timestamp, format="\%Y-\%m-\%d \%H:\%M:\%S", tz="UTC"), data=file, proj=CRS("+proj=longlat +ellps=WGS84"))
move(x=1:10,y=1:10,time=as.POSIXct(1:10, origin='1970-1-1'),proj=CRS('+proj=longlat +ellps=WGS84'))
move(x=1:10,y=1:10,time=as.POSIXct(c(1:5,1:5), origin='1970-1-1'),proj=CRS('+proj=longlat +ellps=WGS84'), animal=c(rep('a',5),rep('b',5)))}
}
