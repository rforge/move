setGeneric("lines") ##need to be different -- 
setMethod("lines", ".MoveTrackSingle", function(x,...){
  lines(coordinates(x), ...)
})

setMethod("lines", ".MoveTrackStack", function(x,col=NA,...){
  if(any(is.na(col)))
    col <- 1:length(unique(x@trackId))
  if(length(col)!=n.locs(x))
    col <- col[as.numeric(x@trackId)] # needs to correspond to points function
  x$col <- col
  unstacked <- split(x)
  l = lapply(unstacked, function(x,...){lines(x, col=x$col, ...)}, ...)
})##marco consioder making here segemnts instead of a splited object

setMethod("lines", ".MoveTrackSingleBurst", function(x,...){
    coords <- coordinates(x)
    segments(x0=coords[-nrow(coords),1], y0=coords[-nrow(coords),2], x1=coords[-1,1], y1=coords[-1,2], col=x@burstId, ...)
    if(length(levels(x@burstId))>8) warning("There are more burst IDs than colors, therefore colors are recycled.")
  }) 
