\name{getMovebankID}

\alias{getMovebankID}
\alias{getMovebankID,character,MovebankLogin-method}
\alias{getMovebankID,character,missing-method}

\docType{methods}

\title{Download data from Movebank}

\description{getMovebankID downloads the location and timestemp columns of an animal of study stored in Movebank. }

\usage{
  getMovebankID(x,login)
}

\arguments{
  \item{x}{character, full name of the study, as stored on Movebank}
  \item{login}{...}
}

\details{
  With the getMovebankID function you can access the location and timestamp data of a particulat animal within a certain study. 
  Remember that you need to be logged in to Movebank, see \code{\link{movebankLogin}}.
}


\author{Marco Smolla}