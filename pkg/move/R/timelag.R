###extract time.lag from Move
if (!isGeneric("time.lag")) {setGeneric("time.lag", function(x, ...) standardGeneric("time.lag"))}
setMethod("time.lag", ".MoveTrackSingle", function(x, ...){
  return(as.numeric(diff(x@timestamps),...)) #one less than locations! we need a more elegant way than just adding a zero 
})

setMethod("time.lag", "MoveStack", function(x, ...){
  return(lapply(split(x), time.lag))
})
