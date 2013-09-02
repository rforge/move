setGeneric("trackId", function(x){standardGeneric("trackId")})
setMethod("trackId", 
          signature=".MoveTrackStack",
          definition=function(x){  
		  return(slot(x,'trackId'))
	  })
