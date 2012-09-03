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
})

