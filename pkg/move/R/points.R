setGeneric("points")
setMethod("points", ".MoveTrackStack", function(x,col=NA,...){
  if(any(is.na(col)))
    col <- 1:length(unique(x@trackId))
  if(all(length(col)!=n.locs(x)))
    col <- col[as.numeric(x@trackId)]# needs to correspond to lines function
  points(coordinates(x), col=col,...)
})

setMethod("points", ".MoveTrackSingle", function(x,...){
  points(coordinates(x), ...)
})

#setMethod("points", ".MoveTrackSingleBurst", function(x,...){
#  points(midPoint(coordinates(x)[-n.locs(x), ], coordinates(x)[-1, ]), col=x@burstId, ...)
#})


 