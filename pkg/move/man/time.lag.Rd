\name{time.lag}

\alias{time.lag}
\alias{time.lag,Move-method}

\docType{methods}

\title{Calculates the time lags between the coordinates}

\description{The time.lag function calculates the time lags between locations.}

\usage{
  \S4method{time.lag}{Move}(x,...)
}

\arguments{
  \item{x}{a Move object}
  \item{...}{further arguments}
}

\details{Optionally the argument units can be passed on to ensure the time lag in a certain unit. For more information on the units argument see the help of difftime.}

\value{
  The function returns numeric vector time lags.
}

\author{Marco Smolla}
