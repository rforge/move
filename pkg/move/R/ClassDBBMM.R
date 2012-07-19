setClass(Class = ".UDStack", contains=c("RasterStack"), 
         representation = representation (
           method = "character"), 
         prototype = prototype(
           method = as.character()),
         validity = function(object){
           if(!all(apply(values(object), MARGIN=2,FUN=function(X) isTRUE(all.equal(sum(X),1))))) 
             stop("One or more of the used rasters are not a UD (sum is not equal to 1)")
         }#check if every layer sums to 1
         )

setClass(Class = ".UD", contains=c("RasterLayer"), 
         representation = representation (
           method = "character"), 
#           parameters = "data.frame"), 
         prototype = prototype(
           method = as.character()
           ),
         validity = function (object){
           if (!isTRUE(all.equal(sum(values((object))),1))) 
		stop("The used raster is not a UD (sum unequal to 1)")
           return(TRUE)
         }
         )

###Defining the class of the Brownian Bridge Movement Model object
setClass(Class = "DBBMMStack",contains=c(".UDStack"),
         representation = representation (
           DBMvar = "dBMvarianceStack", 
           ext = "numeric"), #storing the extent of the map
         prototype = prototype(
           ext = as.numeric()),
         validity = function (object){
           if(!all(unique(object@DBMvar@trackId)==object@layernames))
             stop("The layer names of the raster objects do not match the trackIDs of the DBMvarStack.")
         }#check if names of layers in raster stack match names of individuals in dBMvarStack
         )

setClass(Class = "DBBMM",contains=c(".UD"),
         representation = representation (
           DBMvar = "dBMvariance", 
           ext = "numeric" #storing the extent of the map
           ),
         prototype = prototype(
           ext = as.numeric())#,
#          validity = function(object){           
#            if(is.na(outerProbability(object))){#moved from brownian.bridge.dyn 
#              stop("The used extent is too large. Choose a smaller value for ext!")# when did this occure Marco? 
#            } else {
#              if (outerProbability > .01){
#                warning("outer probability: ", outerProbability," The used extent is too small. Choose an extent which includes more of the probabilities.")
#              } 
#            }           
#          }
         )


setGeneric("brownian.bridge.dyn", function(object,raster=1,dimSize=10,location.error,margin=11, time.step=NULL, window.size=31, ext, bbox=NA, ...){standardGeneric("brownian.bridge.dyn")})

###if neither a raster nor the dimSize is given, then the cell size is calculated by the defauled dimSize and the largest dimension
setMethod(f="brownian.bridge.dyn", 
          signature=c(object="Move",raster="missing", dimSize="missing",location.error="numeric"),
          #signature=c(object="SpatialPointsDataFrame",raster="missing", dimSize="missing",location.error="numeric"),
          function(object, raster, dimSize, location.error,...){
#           print("raster and dimSize missing")
           return(brownian.bridge.dyn(object=object, dimSize=dimSize, location.error=location.error, margin=margin, time.step=time.step, window.size=window.size, var=var,ext=ext,...))
         }) #seems to be necessary


###if no raster object but a dimSize is given, the cell size of the raster is calculated with the number of cells given by the dimSize
#NOTE: the dimSize is a raw estimate of number of cells of the highest range side. it is however not the final number of cells in that direction because when calculating the raster it is extended by the ext factor and there is rounding with ceiling also taking part. 
setMethod(f="brownian.bridge.dyn", 
          signature=c(object="SpatialPointsDataFrame",raster="missing", dimSize="numeric",location.error="numeric"),
          function(object, raster, dimSize, location.error, ...){
            #print("object SPDF, dimSize numeric")
            if (!any(is.na(bbox))) {
              Range <- extent(bbox)
              Range <- c(Range@xmin, Range@xmax, Range@ymin, Range@ymax)
              } else {
                Range <- .extcalc(obj = object, ext = ext)
                }
            yRange <- diff(Range[3:4])
            xRange <- diff(Range[1:2])            
            
            #largest dimension divided by number of cells (=dimSize) gives cell.size (raster="numeric")
            if (xRange > yRange){
              raster <- xRange/dimSize
            } else{
              raster <- yRange/dimSize
            }
            return(brownian.bridge.dyn(object=object, raster=raster, location.error=location.error, margin=margin, time.step=time.step, window.size=window.size, ext=ext,...))
          })

#if there is no valid raster object, it should be calculated     
#make a raster object and feed it (again) to the brownian.bridge.dyn function (now it will call the right function, because raster is now a raster object)   
setMethod(f = "brownian.bridge.dyn",
          signature = c(object="SpatialPointsDataFrame",raster="numeric",dimSize="missing",location.error="numeric"),
          definition = function(object,raster,dimSize,location.error,...){
            #print("object SPDF, raster numeric")
            if (!any(is.na(bbox))) {
              Range <- extent(bbox)
              Range <- c(Range@xmin, Range@xmax, Range@ymin, Range@ymax)
            } else {
              Range <- .extcalc(obj = object, ext = ext)
            }
            yRange <- diff(Range[3:4])
            xRange <- diff(Range[1:2])
            #calculation of the coordinates to fit squared raster cells
            ymin <- Range[3] - (ceiling(yRange/raster) * raster - yRange)/2
            ymax <- Range[4] + (ceiling(yRange/raster) * raster - yRange)/2
            xmin <- Range[1] - (ceiling(xRange/raster) * raster - xRange)/2
            xmax <- Range[2] + (ceiling(xRange/raster) * raster - xRange)/2
            #Calculate the raster; the raster variable replaces here the cell size
            nrow <- ((ymax-ymin)/raster) 
            ncol <- ((xmax-xmin)/raster)
            ex <- extent(c(xmin,xmax,ymin,ymax))
            rst <- raster(ncols=ncol,nrows=nrow, crs=proj4string(object), ex)                            
            return(brownian.bridge.dyn(object=object, raster=rst, location.error=location.error, margin=margin, time.step=time.step, window.size=window.size, var=var,ext=ext,...))
          }
          )

setMethod(f = "brownian.bridge.dyn",
          signature = c(object=".MoveTrackSingle", raster="RasterLayer",dimSize="missing", location.error="numeric"),
          definition = function(object, raster, location.error, ...){        
            #check for aeqd projection of the coordinates
            if (grepl("aeqd",proj4string(object)) == FALSE) {stop("\n The projeciton of the coordinates needs to be \"aeqd\". You may want to use the spTransform funciton to change the projection. \n")} else {}
            
            time.lag <- c(time.lag(object, units="mins"),0)

            if(length(location.error) == 1)
              location.error <- rep(x = location.error, times = n.locs(object))

            DBMvar <- brownian.motion.variance.dyn(object=object, location.error=location.error, margin=margin, window.size=window.size)
            
            if(is.null(time.step)==TRUE){ 
              time.step <- (min(time.lag[-length(time.lag)])/15)
            }
            
            T.Total <- sum(time.lag[DBMvar@interest])

            compsize <- ncell(raster)*(sum(time.lag[DBMvar@interest])/time.step)
            print(paste("Computational size:", sprintf("%.1e", compsize)))
	    # this is not needed anymore since now the routine can be interuped
       #     if (compsize>500000000){
       #       cat("The calculation may take longer than 5 minutes. \n")
       #       cat("If you don't want to proceed, abort the function now! \n")
       #       cat("Process continues within 10 seconds. \n")
       #       pb <- txtProgressBar(min = 0, max = 100, style = 1)
       #       for(i in 1:100){
       #         Sys.sleep(.1)# Waiting 10s
       #         setTxtProgressBar(pb, i)
       #       }
       #       close(pb)
       #     }
                        
            interest <- (c(DBMvar@interest, 0)+c(0, DBMvar@interest))[1:length(DBMvar@interest)]!=0

            ans <- .Fortran("dBBMM",
                            as.integer(1+sum(DBMvar@interest)), #n.locs
                            as.integer(ncell(raster)), #gridSize
                            as.double(c(time.lag[DBMvar@interest],0)), #timeDiff
                            as.double(T.Total), #total time
                            as.double(coordinates(object)[interest,1]), # x coordinates track
                            as.double(coordinates(object)[interest,2]), # y coordinates track
                            as.double(c(DBMvar@means[DBMvar@interest],0)), # variance estimates
                            as.double(location.error[interest]), 
                            as.double(coordinates(raster)[,1]), # raster coordinates
                            as.double(coordinates(raster)[,2]),
                            as.double(time.step), 
                            as.double(rep(0, ncell(raster))))# probability vector to be returned
            
            raster <- setValues(raster, ans[[12]])
            
            dBBMM <- new("DBBMM",
                  			 DBMvar = DBMvar, 
                  			 method = "Dynamic Brownian Bridge Movement Model",
                  			 raster, 
                  			 ext = ext)
            outerProbability <- outerProbability(dBBMM)
            
            if(is.na(outerProbability)){
              stop("The used extent is too large. Choose a smaller value for ext!")# when did this occure Marco? # should we move these checks to the validity function of the dbbbmm object
            } else {
              if (outerProbability > .01){
                warning("outer probability: ", outerProbability," The used extent is too small. Choose an extent which includes more of the probabilities.")
              } 
            }

            return(dBBMM)
          }
          )

###do brownian.bridge.dyn for all individuals within a MoveStack
setMethod(f="brownian.bridge.dyn", 
          signature=c(object="MoveStack",raster="RasterLayer", dimSize="missing",location.error="numeric"),
          function(object, raster, dimSize, location.error,...){
            #.extcalc already calculated the right raster extension for all tracks
            rm(dimSize)   ##is not needed anymore, because RasterLayer is already calculated
            moveUnstacked <- split(x=object) #split MoveStack into individual Move objects
            dbbmmLST  <- list()
            omitMove <- c()
            for (i in names(moveUnstacked)){
              if (nrow(moveUnstacked[[i]]@coords) > (window.size+margin)){
                dbbmmLST[[i]] <- brownian.bridge.dyn(moveUnstacked[[i]], raster=raster,..., location.error=location.error, margin=margin, time.step=time.step, window.size=window.size, var=var,ext=ext)
                #          dbbmmLST[[i]]  <- dbbmm
              } else {omitMove <- c(omitMove,i)} #store which Move Objects were not processed
            }
            if (length(omitMove)>0) 
              warning("Move object ",paste(omitMove,collapse=" ")," was/were omitted, because the number of coordinates is smaller than the window.size and margin you use.\n")      
            
            #rasterStack <- do.call('stack',dbbmmLST)
            rasterStack <- stack(lapply(dbbmmLST, as, 'RasterLayer'))
            DBMvarLST <- lapply(dbbmmLST, slot,'DBMvar')
            objectAnimalsOmitted <- object[as.character(object@trackId)%in%names(DBMvarLST)]
            dBMvarianceStack <- new("dBMvarianceStack", objectAnimalsOmitted,
                                    in.windows = unlist(lapply(DBMvarLST,slot,"in.windows")), 
                                    interest = unlist(lapply(DBMvarLST,slot,"interest")), 
                                    means = unlist(lapply(DBMvarLST,slot,"means")), 
                                    margin = unique(unlist(lapply(DBMvarLST,slot,"margin"))), #bart break lst should still be inhereted here
                                    window.size = unique(unlist(lapply(DBMvarLST,slot,"window.size")))
            )
            DBBMMStack <- new("DBBMMStack",DBMvar=dBMvarianceStack, rasterStack)
            return(DBBMMStack)
          })



setGeneric(".extractDBMvar", function(object){standardGeneric(".extractDBMvar")})
setMethod(f=".extractDBMvar", signature="list", definition=function(object){  
  new()
  means <- unlist(lapply(DBMvarLST,slot,"means"))
  in.windows <- c(in.windows, object[[i]]@DBMvar@in.windows)
  interest <- c(interest, object[[i]]@DBMvar@interest)
  
  
  return(dBMvarianceStack)  
})


setGeneric(".extractDBMvar", function(object){standardGeneric(".extractDBMvar")})
setMethod(f=".extractDBMvar", signature="DBBMM", definition=function(object){  return(object@DBMvar)  })

#calculating the extent ##works for Move and Movestack (both inherit SPDF)
setGeneric(".extcalc", function(obj, ext) standardGeneric(".extcalc"))
setMethod(f = ".extcalc", 
          signature = c(obj="SpatialPointsDataFrame",ext="numeric"), 
          definition = function(obj,ext){
            Range <- as.vector(c(obj@bbox[1,],obj@bbox[2,]))
            if(length(ext)==1) {
              ext <- rep(ext, 4)
            } else {}            
            if(length(ext)==2) {
              ext <- rep(ext, each = 2) 
            } else {}                      
            if(length(ext)==4) { 
              yRange <- c(Range[3]-abs(diff(Range[3:4])*ext[3]), Range[4]+abs(diff(Range[3:4])*ext[4]))
              xRange <- c(Range[1]-abs(diff(Range[1:2])*ext[1]), Range[2]+abs(diff(Range[1:2])*ext[2]))
            } else {stop("The ext argument must be 1, 2 or 4 numbers")}
            return(c(xRange, yRange))
          }
          )


#if (!isGeneric("outerProbability")){
setGeneric("outerProbability", function(raster, border=.1){standardGeneric("outerProbability")})
#}

setMethod(f = "outerProbability",
          signature = c(raster="RasterLayer"),
          definition = function(raster, border){
            rowRange <- ceiling(nrow(raster)*border)
            colRange <- ceiling(ncol(raster)*border)
            innerProbability <- sum(getValuesBlock(x=raster,row=rowRange, nrows=nrow(raster)-rowRange, col=colRange,ncols=ncol(raster)-colRange))
            outerProbability <- cellStats(raster,stat=sum)-innerProbability          
            return(outerProbability/cellStats(raster,stat=sum))
          }  
          )

####################
## Plotting dbbmm ##
####################
#setMethod(f = "plot",
#          signature = "DBBMM",
#          definition = function(x){ #maybe some more variables for the desgin
#            plot(raster(x))
#          }
#          ) 
#
#setGeneric("image")
#setMethod(f = "image",
#          signature = "DBBMM",
#          definition = function(x,col=rainbow(356),...){ #maybe some more variables for the desgin
#            image(raster(x),col=col,...)
#          }
#          )

setGeneric("contour", function(x,...) standardGeneric("contour"))
setMethod(f = "contour",
          signature = c(x=".UD"), ## enter nlevel for the number of levels, or levels for the correct levels!!
          definition = function(x, ...){
            newRaster <- x#raster2contour(x, ...)
            rank <- (1:length(values(newRaster)))[rank(values(newRaster))]
            values(newRaster) <- 1-cumsum(sort(values(newRaster)))[rank]
            
            x <- newRaster
            callNextMethod() 
          })
            #return the contour as SLDF object: plot=F, google=F
          #  if (plot==F && google==F && track==F){
           #   raster2contour(x,...)
              #return(newRaster)
          #  } else { 
              #browser()
                #plot the contour line: plot=T, google=F
#                 if (plot==T && google==F){
#                 contour(x = newRaster, add = add, ...)
#                 }
                #add track to contour+google map: plot=T, google=T
#                 if (google==T){
#                   print(newRaster)
#                   if (class(y)!="Move"){stop("y must be the corresponding Move object to the DBBMM object")}
#                   if (grepl(pattern="longlat", x=proj4string(y))==FALSE){stop("Use a Move object with longlat projection method")}
#                   require(RgoogleMaps)
#                   lon <- coordinates(y)[,1]
#                   lat <- coordinates(y)[,2]
#                   lonRange <- c(min(range(lon))-abs(diff(range(lon))*x@ext[1]), max(range(lon))+abs(diff(range(lon))*x@ext[2]))                 
#                   latRange <- c(min(range(lat))-abs(diff(range(lat))*x@ext[3]), max(range(lat))+abs(diff(range(lat))*x@ext[4]))
#                   MyMap <- GetMap.bbox(lonR=lonRange, latR=latRange)
# #                  MyMap <- GetMap.bbox(lonR=range(coordinates(y)[ ,1]), latR=range(coordinates(y)[ ,2]))
#                   rst1 <- raster2contour(x, ...)
#                   sldf <- spTransform.SpatialLinesDataFrame(rst1, CRSobj=CRS("+proj=longlat"))
#                   add <- F
#                   for (i in 1:length(sldf@lines)){
#                     for (j in 1:length(sldf@lines[[i]]@Lines)){
#                       sldflon <- sldf@lines[[i]]@Lines[[j]]@coords[,1]
#                       sldflat <- sldf@lines[[i]]@Lines[[j]]@coords[,2]
#                       PlotOnStaticMap(MyMap=MyMap, lon=sldflon, lat=sldflat, FUN=lines,add=add, col=col, lwd=lwd)
#                       add <- T
#                     }
#                   }
#                   #add a track: track=T
#                   if (track==T) {
#                     PlotOnStaticMap(MyMap=MyMap, lon=coordinates(y)[,1], lat=coordinates(y)[,2], add=TRUE, FUN=lines, lwd=llwd, col=lcol)
#                   }
#                   file.remove(paste(getwd(),"MyTile.png",sep="/"))
#                   file.remove(paste(getwd(),"MyTile.png.rda",sep="/"))
#                 }
#                }})

### Contour to SpatialLinesDataFrame conversion
#if (!isGeneric("outerProbability")){
setGeneric("raster2contour", function(x, ...){standardGeneric("raster2contour")}) #bart kami do we want to skip maxpixels? if not than there would be no reason to have a different function name to rasterToContour
#}
setMethod(f = "raster2contour",
          signature = c(x="RasterLayer"),
          definition = function(x, ...){
            rank <- (1:length(values(x)))[rank(values(x))]
            values(x) <- 1 - cumsum(sort(values(x)))[rank]
            rasterToContour(x, ...)
          }
          )

### SUMMARY FOR THE DBBMM OBJECT
###
setGeneric("summary")
setMethod(f = "summary",
          signature = c(object="DBBMM"),
          definition = function(object){
            cat("Raster projection: ",object@crs@projargs,"\n")
            cat("Raster extent \n")
            print(object@extent)
            cat("Raster maximum: ",maxValue(object),"\n")
            cat("Raster minimum: ",minValue(object),"\n")
          }
          )

