\name{move}

\alias{move}
\alias{move,character-method}

\docType{methods}

\title{
Create a Move object
}

\description{
The move method creates a Move object from a movebank file. Additional the projection method of the data can be changed with the 'proj' argument.
}

\usage{
\S4method{move}{character}(x, proj, ...)
}

\arguments{
\item{x}{Full path to the file location}
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