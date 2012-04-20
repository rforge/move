\name{DBBMM-class}

\alias{DBBMM-class}

\docType{class}

\title{
The DBBMM class
}

\description{
The DBBMM object is created within the brownian.bridge.dyn function from a Move object. It includes beneath others a raster object, and probabilities.
}

\section{Slots}{
  \describe{
    \item{DBMvar}{Object of class \code{"DBMvar"}: includes the break.list and points of interest, ...; }
    \item{raster}{Object of class \code{"character"}: the raster object with probability values  }
  }
}

\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "DBBMM")}: print a summary of all data stored in the DBBMM object }
  }
}


\arguments{
...
}

\author{Marco Smolla}

\keyword{classes}