setGeneric("plot") 
setMethod(f = "plot", 
          signature = c(x=".MoveTrackSingle", y="missing"), 
          function(x, y, ...){
            plot(coordinates(x), ...)
          })

setMethod(f = "plot", 
          signature = c(x=".MoveTrackStack", y="missing"), 
          function(x, y, type='p',...){
            plot(coordinates(x), type='n')
            if(type %in% c('p','o','b'))
              points(x,...)
            if(type %in% c('l','o','b'))
              lines(x,...)        
          })

setMethod(f = "plot", 
          signature = c(x=".MoveTrackSingleBurst", y="missing"), 
          function(x, y, ...){
            coords <- coordinates(x)
            plot(coords, type="n")
            segments(x0=coords[-nrow(coords),1], y0=coords[-nrow(coords),2], x1=coords[-1,1], y1=coords[-1,2], col=x@burstId, ...)
            if(length(levels(x@burstId))>8) warning("There are more burst IDs than colors, therefore colors are recycled.")
          }) #make a individual lines and points function and bundle it in plot

