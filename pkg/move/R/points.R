setGeneric("points")
setMethod("points", ".MoveTrackSingle", function(x,add=TRUE,...){
  if (add==FALSE) {rm(add)
                   plot(coordinates(x), type="p", ...)}
  else {points(coordinates(x), type="p", ...)}
})
