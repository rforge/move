\name{outerProbability}

\alias{outerProbability}
\alias{outerProbability,RasterLayer-method}
\alias{outerProbability,DBBMMStack-method}

\docType{methods}

\title{Calculates the probabilities at the edges of a raster}

\description{The outerProbability method calculates the summed probabilitiy of the cells at the border of a raster}

\usage{
  \S4method{outerProbability}{RasterLayer}(raster,border,...)
  \S4method{outerProbability}{DBBMMStack}(raster,border,...)
}

\arguments{
  \item{raster}{a RasterLayer or DBBMMStack object that has values for the raster cells}
  \item{border}{numeric from 0 to 1; ratio of the number of columns at the border relative to the whole raster from which the probabilities should be summed up; default is 10\% (0.1)}
  \item{...}{hand over information}
}

\value{
The function returns either a numeric value with the outer probability from a single raster object, or a list with the outer probabilites from a DBBMMStack.
}

\examples{
load(system.file("extdata", "leroy.RData", package="move"), .GlobalEnv)

#calculate the probabilities of 20\% of the raster at the border from a DBBMM
outerProbability(dbbmm, border=.2)
"[1] 0.001669491"

#calculate the probabilities of 50\% of the raster at the border from a DBBMMStack
outerProbability(dbbmm2, border=.5)
"$Leroy"
"[1] 0.6739206"

"$Leroy1"
"[1] 0.6739206"
}
\author{Marco Smolla}
