setGeneric("plot") 
setMethod(f = "plot", 
          signature = c(x=".MoveTrackSingle", y="missing"), 
          function(x, y, ...){
            plot(coordinates(x), ...)
          })

setMethod(f = "plot", 
          signature = c(x="MoveStack", y="missing"), 
          function(x, y, col=NA, ...){
            if(any(is.na(col)))
              col <- 1:length(unique(x@trackId))
            if(length(col)!=n.locs(x))
              col <- col[as.numeric(x@trackId)]
            x$col <- col
            unstacked <- split(x)
            lines(unstacked[[1]], col=x$col, xlim=c(min(coordinates(x)[,1]),max(coordinates(x)[,1])), ylim=c(min(coordinates(x)[,2]), max(coordinates(x)[,2])), add=F, ... ) #create first plot
            l = lapply(unstacked[-1], function(x,...){lines(x, col=x$col, add=T, ...)}, ...)
          })
