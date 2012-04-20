\name{leroy_fisher_LaPoint}

\alias{leroy_fisher_LaPoint}

\docType{data}

\title{Data set to test the move package}

\description{This file includes spatial data from a fisher. It can be used to test the different functions from the move package. The file is an example of how data are stored at the movebank database.}

\examples{
## create a Move object from the data set
data <- move(system.file("data/leroy_fisher_LaPoint.csv",package="move"),proj=CRS("+proj=longlat"))
}

\author{Marco Smolla}