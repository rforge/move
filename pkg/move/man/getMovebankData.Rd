\name{getMovebankData}

\alias{getMovebankData}
\alias{getMovebankData,ANY,ANY,missing-method}
\alias{getMovebankData,character,ANY,MovebankLogin-method}
\alias{getMovebankData,numeric,ANY,MovebankLogin-method}
\alias{getMovebankData,numeric,character,MovebankLogin-method}
\alias{getMovebankData,numeric,missing,MovebankLogin-method}

\docType{methods}

\title{Download data from Movebank}

\description{getMovebankData downloads the location and timestemp columns of an animal of study stored in Movebank and creates a Move object}

\usage{
  getMovebankData(study, animalName, login, moveObject = TRUE, ...)
}

\arguments{
  \item{study}{character, full name of the study, as stored on Movebank}
  \item{animalName}{chracter, name of the individual as stored on Movebank}
  \item{login}{a \code{\link[=MovebankLogin-class]{MovebankLogin}}, if empty you'll be asked to enter your username or password}
  \item{moveObject}{logical, if FALSE a data.frame with coordiantes and timestamps is returned. Default is TRUE}
  \item{...}{passing on additional arguments}
}

\details{
  With the getMovebankData function you can access the location and timestamp data of a particular animal within a certain study. 
  By default \code{moveObject} is \code{TRUE}. Thus the function returns a \link[=Move-class]{Move} object if an \code{animalName} is given. If there is no \code{animalName}, the whole study is downloaded and stored as a \link[=MoveStack-class]{MoveStack} object.
  Remember that you need an account at Movebank.org, see \code{\link{movebankLogin}}.
}

\note{
See the 'browesMovebank' vignette (\href{http://move.r-forge.r-project.org/moveWEB/Downloads.html}{move website download section}) for more information about security and how to use Movebank from within R. 
}


\author{Marco Smolla}