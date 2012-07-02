\name{DBBMMStack-class}

\alias{DBBMMStack}
\alias{DBBMMStack-class}

\docType{class}

\title{
  The DBBMMStack class
}

\description{
  The DBBMMStack object is created within the brownian.bridge.dyn function from a Move object. It includes beneath others a raster object and probabilities.
}

\section{Slots}{
  \describe{
    \item{DBMvar}{Object of class \code{"dBMvariance"}: includes the break.list and points of interest, ...; }
    \item{crs}{  }
    \item{ext}{the extension factor set by the user}
    \item{extent}{  }
    \item{filename}{  }
    \item{layers}{  }    
    \item{layernames}{  }
    \item{method}{  }
    \item{ncols}{  }
    \item{nrows}{  }
    \item{rotated}{  }
    \item{rotation}{  }
    \item{title}{  }
    \item{z}{  }
  }
}

\section{Methods}{
  \describe{
    \item{\code{\link{contour}}}{\code{signature(object = "DBBMMStack")}: adds a contour line to a plot}
    \item{\code{\link{image}}}{\code{signature(object = "DBBMMStack")}: plots the raster from a DBBMMStack object with fixed cell size ratio}
    \item{\code{\link{plot}}}{\code{signature(object = "DBBMMStack")}: plots the raster from a DBBMMStack object with resize insenstive proportions}
    \item{\code{\link{proj4string}}}{\code{signature(object = "DBBMMStack")}: extracts the projection method of the raster stored within the DBBMMStack object}
    \item{\code{\link{raster}}}{\code{signature(object = "DBBMMStack")}: extracts the raster from the DBBMMStack object}
    \item{\code{\link{outerProbability}}}{\code{signature(object = "DBBMMStack")}: calculates the animal occurence probabilities at the border of the raster}
  }
}

\author{Marco Smolla}

\keyword{classes}