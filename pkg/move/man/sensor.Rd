\name{sensor}
\alias{sensor}
\alias{sensor,.MoveTrack-method}
\alias{sensor,.unUsedRecords-method}

\docType{methods}

\title{Extract the sensor of a Move or MoveStack object}

\description{The sensor method returns or sets the sensor of a track from a Move or MovesStack object.}

\usage{
\S4method{sensor}{.MoveTrack}(this,...)
\S4method{sensor}{.unUsedRecords}(this,...)
}

\arguments{
  \item{this}{Move or MoveStack object}
  \item{...}{Curently not used}
}

\author{Bart Kranstauber}

\examples{
data(leroy)
sensor(leroy) #get the sensor from a Move object
data(fishers)
sensor(fishers) #get the sensor from a MoveStack object
}
