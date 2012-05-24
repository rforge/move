\name{getMovebankData}

\alias{getMovebankData}
\alias{getMovebankData-method}

\docType{methods}

\title{Download data from Movebank}

\description{getMovebankData downloads the location and timestemp columns of an animal of study stored in Movebank. }

\usage{
  getMovebankData(study,animalName=NA)
}

\arguments{
  \item{study}{character, full name of the study, as stored on Movebank}
  \item{animalName}{chracter, name of the individual as stored on Movebank}
}

\details{
  With the getMovebankData function you can access the location and timestamp data of a particulat animal within a certain study. 
  Remember that you need to be logged in to Movebank, see \code{\link{movebankLogin}}.
}


\author{Marco Smolla}