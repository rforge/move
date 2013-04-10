\name{corridor}

\alias{corridor}
\alias{corridor,.MoveTrackSingle-method}
\alias{corridor,.MoveTrackStack-method}

\docType{methods}

\title{Corridor}

\description{Corridor identifies movement track segments whose attributes suggest corridor use behavior}

\usage{
  \S4method{corridor}{.MoveTrackSingle}(x,speedProp=.75, circProp=.25, plot=FALSE, ...)
  \S4method{corridor}{.MoveTrackStack}(x,speedProp=.75, circProp=.25, plot=FALSE, ...)
}

\arguments{
  \item{x}{Move or MoveStack}
  \item{speedProp}{numeric between 0 and 1, defines the proportion of speeds which are high enough to be a valid corridor point}
  \item{circProp}{numeric between 0 and 1, defines the proportion of trajectories that are low enough to be a valid corridor point}
  \item{plot}{logical, if TRUE the track is plotted together with dots that indicate corridor points (color scale indicates how many corridor points are near by, less: blue, many: pink)}
  \item{...}{additional arguments like levels and nlevels, see details}
}

\details{
  The corridor function uses the attributes of a movement step to identify movement steps that exhibit corridor use behavior. \cr
  For each segment, the speed and the azimuth are calculated and assigned to the segment midpoint. A circular buffer is created around the midpoint of each segment whose radius is equal to half the segment length. The segment azimuth (180 >= azimuth > -180) is then converted into a new unit (the 'pseudo-azimuth' 0 <= 360). Subsequent, the circular variance of the pseudo-azimuths of all segment midpoints that fall within the circular buffer are calculated. This identifies segments that are near parallel segments.  Next, it is determined whether a segment's speed is higher than \code{speedProp} (by default the upper 25\% speeds) and its circular variance is lower than \code{circProp} (by default the lower 25\% of all variances).  Segment midpoints that meet both of these requirements are considered as a 'corridor' point, all others are considered 'non-corridor' points. Finally, a corridor point is determined to be within a true corridor if within its buffer there are more 'corridor' points than 'non-corridor' points.  \cr
  Note: the default values for the \code{speedProp} and \code{circProp} can be changed as per the users discretion using the according argument.\cr
  If the result of the function is assigned to a variable a MoveBurst object is returned (see Value).
}

\value{
  The function returns a MoveBurst object or a list of MoveBurst objects (if a MoveStack is supplied). The MoveBurst@data slot stores the following information: \cr
  - segment midpoint \cr
  - speed \cr
  - azimuth \cr
  - pseudo-azimuth \cr
  - circular variance \cr \cr 
  The object is bursted by the factor that indicates whether a coordinate belongs to a corridor segment or not. 
}

\author{Marco Smolla}

\references{
  LaPoint, S., Gallery, P., Wikelski, M. and Kays, R. (in press) Animal behavior, cost-based corridor models, and real corridors. Landscape Ecology
}

\examples{
  load(system.file("extdata", "move.RData", package="move"), .GlobalEnv)
  
  tmp <- corridor(leroy, plot=TRUE)
  head(tmp) #if assigned to a variable, the coordinates are exported
  
  stack <- moveStack(list(leroy, ricky[1:1000,]))
  stacktmp <- corridor(stack) #working with a stack
}
