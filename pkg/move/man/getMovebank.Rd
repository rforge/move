\name{getMovebank}

\alias{getMovebank}
\alias{getMovebank,character,missing-method}
\alias{getMovebank,character,MovebankLogin-method}

\docType{methods}

\title{Creates an URL to download Data from Movebank}

\description{An enhanced function to download data from Movebank by manually building an URL. This function should only be used by advanced programmers.}

\usage{
  getMovebank(entity_type, login, ...)
}

\arguments{
  \item{entity_type}{the entity_type of the data source}
  \item{login}{a \code{\link[=MovebankLogin-class]{MovebankLogin}}, if empty you'll be asked to enter your username or password}
  \item{...}{passing on additional arguments}
}

\author{Marco Smolla}