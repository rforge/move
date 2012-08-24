setClass(Class = ".MoveTrackSingleBurst", contains = c(".MoveTrackSingle"), 
         representation = representation(
           burstId = "factor"), 
         prototype = prototype(
           burstId = factor()), 
    validity = function(object) {
	    if(length(object@burstId)!=(length(object@timestamps)-1))
		    stop("Burst ids need to be one shorter than rest since it is a segment property")
        return(TRUE)
      })

setClass(Class = "MoveBurst", contains = c(".MoveTrackSingleBurst", ".MoveGeneral"), 
    validity = function(object) {
        return(TRUE)
    })

setGeneric("burst", function(x, f, ...) {standardGeneric("burst")})
setMethod("burst", 
          signature=c(x = "Move", f = "factor"), 
          definition = function(x, f, ...) {
          new("MoveBurst", 
              as(x, ".MoveTrackSingle"), 
              as(x, ".MoveGeneral"), 
              burstId = f, 
              idData = x@idData)
          })

setMethod("burst", 
          signature=c(x = "Move", f = "numeric"), 
          definition = function(x, f, ...) {
            burst(x = x, f = as.factor(f))
          })

setMethod("burst", c(x = "Move", f = "character"), definition = function(x, f, ...) {
    if (length(f) == 1) {
        burst(x = x, f = do.call("$", list(x, f))[-n.locs(x)])
    } else {
        burst(x = x, f = as.factor(f))
    }
    
})
setMethod("[", signature(x = ".MoveTrackSingleBurst"), function(x, i, j, ...) {
    if (!missing(i)) {
	tmp<- x@burstId[i]
        x@burstId <-tmp[-length(tmp)]
    } else {
        i <- T
    }
    if (missing(j)) 
        j <- T
    if (class(i) == "character") 
        stop("Not sure if these methods work for class character")
    if (class(j) == "character") 
        stop("Not sure if these methods work for class character")
    callNextMethod(x = x, i = i, j = j, ...)
})
setMethod("split", signature = c(x = ".MoveTrackSingleBurst", f = "missing"), definition = function(x, 
    f, ...) {
    
    f <- c(0, cumsum(diff(as.numeric(factor(paste(as.character(x@burstId), is.na(x@burstId))))) != 
        0))
	f<-c(f, max(f))
    res <- list()
    for (i in unique(f)) 
	    res[[i + 1]] <- x[f == i | c(0, f[-n.locs(x)]) == i, ]
    names(res) <- as.character(unlist(lapply(lapply(res, slot, "burstId"), "[", 1)))
    res<-lapply(res, as, sub("Burst", "", class(x)))
    return(res)
})
setAs("MoveBurst", "Move", function(from) {
# last id can be different since that one is not telling anything about a segment in this obj
    if (length(unique(from@burstId)) != 1) 
        stop("Not one unique burst id method wont work")
    new("Move", as(from, ".MoveGeneral"), as(from, ".MoveTrackSingle"))
}) 
