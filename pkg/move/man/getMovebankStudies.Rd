\name{getMovebankStudies}

\alias{getMovebankStudies}
\alias{getMovebankStudies,missing,MovebankLogin-method}
\alias{getMovebankStudies,missing,missing-method}

\docType{methods}

\title{Returns all studies available on Movebank}

\description{Returns all studies available on Movebank}

\usage{
  getMovebankStudies(x, login)
}

\arguments{
  \item{x}{a character string (study name) or the numeric study ID as it is stored on Movebank}
  \item{login}{...}
}

\details{
  use a space infront of your login variable, like: \code{getMovebankStudies( ,login)} 
}


\author{Marco Smolla}