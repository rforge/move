setGeneric("plot") 
setMethod(f = "plot", 
          signature = c(x=".MoveTrackSingle", y="missing"), 
          function(x, y, ...){
            plot(coordinates(x), ...)
          })

setMethod(f = "plot", 
          signature = c(x=".MoveTrackStack", y="missing"), 
          function(x, y, type='p',...){
   #         if(any(is.na(col)))
  #            col <- 1:length(unique(x@trackId))
  #          if(length(col)!=n.locs(x))
   #           col <- col[as.numeric(x@trackId)]
    #        x$col <- col
    #        unstacked <- split(x)
            plot(coordinates(x), type='n')
            if(type %in% c('p','o','b'))
              points(x,...)
    #        l = lapply(unstacked, function(x,...){points(x, col=x$col, ...)}, ...)
            if(type %in% c('l','o','b'))
              lines(x,...)
    #        l = lapply(unstacked, function(x,...){lines(x, col=x$col,...)}, ...)            
          })
