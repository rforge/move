###extract time.lag from Move
if (!isGeneric("time.lag")) {setGeneric("time.lag", function(x, ...) standardGeneric("time.lag"))}
setMethod("time.lag", ".MoveTrackSingle", function(x, ...){
  return(as.numeric(diff(timestamps(x)),...)) #one less than locations! we need a more elegant way than just adding a zero 
})

setMethod("time.lag", ".MoveTrackStack", function(x, ...){
  return(lapply(lapply(split(timestamps(x), x@trackId), diff),as.numeric,...))
})
