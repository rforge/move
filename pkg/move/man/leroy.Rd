\name{leroy}

\alias{leroy}

\docType{data}

\title{GPS track data from a fisher}

\description{This file includes spatial data from a fisher (\emph{Martes pennanti}). It can be used to test the different functions from the move package. \cr 
These location data were collected via a 105g GPS tracking collar (manufactured by E-obs gmbh) and programmed to record the animal's location every 15 minutes, continuously. The collar was deployed from 10 February 2009 through 04 March 2009 on an adult, resident, male fisher, in New York, USA.  Scott LaPoint tracked this fisher for part of his dissertation research hoping to better understand how fishers were moving throughout this developed landscape.}

\references{
For more information, contact Scott LaPoint \email{sdlapoint@gmail.com}
}

\examples{
## create a Move object from the data set
data <- move(system.file("extdata","leroy.csv",package="move"))
plot(data)

}

\author{Marco Smolla}
