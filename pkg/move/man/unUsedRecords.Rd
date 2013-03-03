\name{'unUsedRecords<-'}

\alias{unUsedRecords<-}
\alias{unUsedRecords}
\alias{unUsedRecords<-,.MoveTrackSingle,logical-method}
\alias{unUsedRecords<-,.MoveTrackStack,logical-method}
\alias{unUsedRecords,.unUsedRecords-method}
\alias{unUsedRecords,.unUsedRunUsedRecordsStack-method}

\title{Creates unused records data}

\description{
  A function that assigns locations as unused or returns the unUsedRecords part of the object it could for example be used to remove locations test locations from a track. 
}

\usage{
  \S4method{unUsedRecords}{.MoveTrackSingle,logical}(obj) <- value
  \S4method{unUsedRecords}{.MoveTrackStack,logical}(obj) <- value
  \S4method{unUsedRecords}{.MoveTrackSingle}(obj)
  \S4method{unUsedRecords}{.MoveTrackStack}(obj)
}

\arguments{
  \item{obj}{a Move or MoveStack object}
  \item{value}{A logical vector of the same length as the number of locations}
}
\examples{
data <- move(system.file("extdata","leroy.csv.gz",package="move"))
par(mfrow=2:1)
plot(data, type='b')
unUsedRecords(data)<-as.logical((1:n.locs(data))\%\%2)
plot(data, type='b')
}


\author{Marco Smolla}
