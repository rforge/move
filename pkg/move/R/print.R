###Print function for a Move and MoveStack object
setGeneric("print")
# setMethod("print",".MoveTrackStack",function(x){
#           callNextMethod(x)
#           if (exists("study.name",x@idData)==TRUE){
#             cat("study name  :",levels(x@idData$study.name),"\n")}
#           if (exists("individual.taxon.canonical.name", where=x@idData)==TRUE){
#             cat("species     :",as.character(unique(x@idData$individual.taxon.canonical.name)),"\n")}
#           cat("no. of indiv:",nlevels(x@trackId),"\n")
#           cat("indiv. ids  :",paste(levels(x@trackId),collapse=", "),"\n")
#           pp <- split(x@coords,x@trackId)
#           cat("no. of fixes:",unlist(lapply(pp,length)),"\n")
#           }
#           )
# 
# setMethod("print","MoveStack",
#           function(x){
#             callNextMethod(x)
#             #if (exists("sensor.type", where=x@idData)==TRUE){
#             #  cat("sensor type :",levels(x@idData$sensor.type),"\n")}
#             maxItems <- 10  
#             items <- ncol(x@idData)
#             if (items > maxItems) { 
#               coln <- colnames(x@idData)
#               coln <- c(coln[1:maxItems], '...')
#             } else {coln <- colnames(x@idData)}
#               cat("indiv. attr.:", paste(coln, collapse=", "), "\n")
#           }
#           )

setMethod("print",".MoveTrackSingle",
          function(x){
            #callNextMethod(x)
            cat("Class        :", class(x),"\n")
            try(cat("name         :", rownames(x@idData),"\n"),silent=T)
            cat("nfeatures    :", nrow(coordinates(x)),"\n")
            cat("extent       :", c(extent(x)@xmin, extent(x)@xmax, extent(x)@ymin, extent(x)@ymax),"\n")
            cat("coord.ref    :", proj4string(x),"\n")
            cat("ndatacols    :", ncol(x@data),"\n")
            cat("variables    :", paste(colnames(x@data),collapse=", "), "\n")
            timeRange <- range(x@timestamps)
            cat("timestamps   :",paste(timeRange, collapse="..."),"\n")
            cat("duration     :", capture.output(round(difftime(timeRange[2],timeRange[1]))), "\n")
            try(silent=TRUE, if(length(x@timesMissedFixes)>=1)
              cat("missed fixes :", length(x@timesMissedFixes)) )
          }
          )

# setMethod("print", ".MoveTrack", function(x){
#   print(as(x[x@trackId==ID,],"SpatialPointsDataFrame"))
#   callNextMethod(x)
#   timeRange <- range(x@timestamps)
#   cat("timestamps  :",paste(timeRange, collapse="..."),capture.output(round(difftime(timeRange[2],timeRange[1]))), " (start...end, duration) \n")  
# }       
# )

##Print function for a Move and MoveStack object
setGeneric("print")
setMethod("print",".MoveTrackStack",function(x){
  #callNextMethod(x)
  cat("Class        :", class(x),"\n")
  cat("nfeatures    :", length(coordinates(x)[,1]),"\n")
  cat("extent       :", c(extent(x)@xmin, extent(x)@xmax, extent(x)@ymin, extent(x)@ymax),"\n")
  cat("coord.ref    :", proj4string(x),"\n")
  cat("ndatacols    :", ncol(x@data),"\n")
  cat("variables    :", paste(colnames(x@data),collapse=", "), "\n")
  if (exists("study.name",x@idData)==TRUE){
    cat("study name   :",levels(x@idData$study.name),"\n")}
  if (exists("individual.taxon.canonical.name", where=x@idData)==TRUE){
    cat("species      :",as.character(unique(x@idData$individual.taxon.canonical.name)),"\n")}
  cat("no. of indiv :",nlevels(x@trackId),"\n")
  cat("indiv. ids   :",paste(levels(x@trackId),collapse=", "),"\n")
  pp <- split(x@coords[,1],x@trackId)
  cat("no. of fixes :",unlist(lapply(pp,length)),"\n")
})

setMethod("print","MoveStack",
          function(x){
            callNextMethod(x)
            try(silent=TRUE, if(length(x@timesMissedFixes)>1)
              cat("missed fixes  :", length(x@timesMissedFixes)) )
            if (exists("sensor.type", where=x@idData)==TRUE){
              cat("sensor type  :",levels(x@idData$sensor.type),"\n")}
            maxItems <- 10  
            items <- ncol(x@idData)
            if (items > maxItems) { 
              coln <- colnames(x@idData)
              coln <- c(coln[1:maxItems], '...')
            } else {coln <- colnames(x@idData)}
            cat("indiv. attr. :", paste(coln, collapse=", "), "\n")
            timeRange <- range(x@timestamps)
            cat("timestamps   :",paste(timeRange, collapse="..."),"\n")
            cat("duration     :", capture.output(round(difftime(timeRange[2],timeRange[1]))), "\n")
          })
