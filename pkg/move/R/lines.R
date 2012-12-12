setGeneric("lines") ##need to be different -- 
setMethod("lines", ".MoveTrackSingle", function(x,...){
  lines(coordinates(x), ...)
})

setMethod("lines", ".MoveTrackStack", function(x,col=NA,...){
  if(any(is.na(col)))
    col <- 1:length(unique(x@trackId))
  if(all(length(col)!=length(x@trackId)-1))
    col <- col[as.numeric(x@trackId)] # needs to correspond to points function
    res <- lapply(levels(x@trackId), function(Id, x, ...) {coords <- coordinates(x)[x@trackId==Id,] 
             segments(x0=coords[-nrow(coords),1], y0=coords[-nrow(coords),2], x1=coords[-1,1], y1=coords[-1,2], col=col[x@trackId==Id], ...)},x=x, ...)
})

setMethod("lines", ".MoveTrackSingleBurst", function(x,col=NA,...){
    coords <- coordinates(x)
    
    
    if (length(col)==1 && is.na(col)) {
      col <- x@burstId
    } else {
      if (length(col)==length(unique(x@burstId))){
        col <- col[as.numeric(x@burstId)] # needs to correspond to points function
      } else {
        stop("The number of assigned colors is unequal to the number of burst IDs")
      }
    }
    if(length(levels(x@burstId))>8) warning("There are more burst IDs than colors (recycling colors).")
    segments(x0=coords[-nrow(coords),1], y0=coords[-nrow(coords),2], x1=coords[-1,1], y1=coords[-1,2], col=col, ...)
  }) 
