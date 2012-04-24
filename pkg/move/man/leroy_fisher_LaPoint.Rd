\name{leroy_fisher_LaPoint}

\alias{leroy_fisher_LaPoint}

\docType{data}

\title{Data set to test the move package}

\description{This file includes spatial data from a fisher. It can be used to test the different functions from the move package. \cr 
These location data were collected via a 105g gps tracking collar (manufactured by E-obs gmbh) and programmed to record the animals location every 15 minutes, continuously. The collar was deployed from 10 February 2009 through 04 March 2009 on an adult, resident, male fisher (\emph{Martes pennanti}), near Albany, New York, USA.  Scott LaPoint tracked this fisher for part of his dissertation research hoping to better understand how fisher were moving throughout this developed landscape.}

\references{
For more information, contact Scott LaPoint \email{sdlapoint@gmail.com}
}

\examples{
\dontrun{
## create a Move object from the data set
data <- move(system.file("data/leroy_fisher_LaPoint.csv",package="move"),proj=CRS("+proj=longlat"))
}
}

\author{Marco Smolla}