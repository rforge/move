\name{ricky}

\alias{ricky}

\docType{data}

\title{GPS track data from a fisher}

\description{This file includes spatial data from a fisher (\emph{Martes pennanti}). It can be used to test the different functions from the move package. \cr 
These location data were collected via a GPS tracking collar (manufactured by E-obs GmbH) and programmed to record locations depending on the animal's behaviour (up to a one location every two minutes). The collar was deployed on an adult, resident, male fisher, in New York, USA (see References). The data usage is permitted for exploratory purposes. For other purposes please get in contact.}

\references{
For more information, contact Scott LaPoint \email{sdlapoint@gmail.com}
}

\examples{
## create a Move object from the data set
data <- move(system.file("extdata","ricky.csv.gz",package="move"))
plot(data)

}

\author{Scott LaPoint}
