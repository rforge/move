\name{move}

\alias{move}
\alias{move,character-method}
\alias{move,numeric-method}

\docType{methods}

\title{
Create a Move object
}

\description{
The move method creates a Move object from a movebank file. Additional the projection method of the data can be changed with the 'proj' argument.
}

\usage{
\S4method{move}{character}(x, proj)
\S4method{move}{numeric}(x, y, time, data, proj)%proj = CRS("+proj=longlat +ellps=WGS84"), ...)
}

\arguments{
\item{x}{Full path to the file location, or column indicator for non-movebank data for the x coordinates; e.g. \code{data$x}}
\item{y}{column indicator for non-movebank data for the y coordinates}
\item{time}{column indicator for non-movebank data for the time stamps}
\item{data}{full parh of the location of the non-movebank file}
\item{proj}{projection method; requires a valid CRS object}
\item{...}{Additional arguments}  
}

\details{
\code{x} has to be a file downloaded from movebank.org. The projection method is \code{proj=CRS("+proj=longlat +ellps=WGS84")} as default.
}


\author{Marco Smolla }

\examples{

\dontrun{
test <- move(x=FILE)
test <- move(x=FILE, proj=CRS("+proj=longlat"))
}
}