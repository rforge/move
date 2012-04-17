\name{show}

\alias{show,Move-method}

\docType{methods}

\title{Show a Move object}

\description{Displays a summary of a Move object.}

\usage{
  \S4method{show}{Move}(object)
}

\arguments{
  \item{object}{a Move object}
}

\details{
  The show function displays a summary of a Move object. This includes:
    \itemize{
    \item animal ID
    \item species name
    \item study name
    \item number of track points
    \item receiver type
    \item projection method
    \item date of file creation
    \item the first three lines of the spatial data frame
    \item study citation
    \item data licens
}
}

\author{Marco Smolla}