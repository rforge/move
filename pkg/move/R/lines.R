setGeneric("lines") ##need to be different -- 
setMethod("lines", ".MoveTrackSingle", function(x,add=TRUE,...){
  if (add==FALSE) {rm(add)
                   plot(coordinates(x), type="l", ...)}
  else {lines(coordinates(x), type="l", ...)}
})
