setGeneric("brownian.bridge.dyn", function(object, raster = 1, dimSize = 10, location.error, margin = 11, window.size = 31, ext=.3, bbox = NA, ...) {
  standardGeneric("brownian.bridge.dyn")
})
# This method is to enable pointing at a collumn that contains the location error
setMethod(f = "brownian.bridge.dyn", 
          signature = c(object = ".MoveTrackSingle", raster = "RasterLayer", dimSize = "missing", location.error = "character"), 
          function(object, raster, dimSize, location.error, ...) {
            location.error <- do.call("$", list(object, location.error))
            if(is.null(location.error))
              stop('column indicated for location error probably does not exist')
            brownian.bridge.dyn(object = object, location.error = location.error, raster = raster, ext=ext, ...)
          })

### if neither a raster nor the dimSize is given, then the cell size is
### calculated by the defauled dimSize and the largest dimension
setMethod(f = "brownian.bridge.dyn", 
          signature = c(object = ".MoveTrackSingle", raster = "missing", dimSize = "missing", location.error = "numeric"), 
          function(object, raster, dimSize, location.error, ...) {
            return(brownian.bridge.dyn(object = object, dimSize = dimSize, location.error = location.error, margin = margin, window.size = window.size, ext = ext, ...))
          })  #seems to be necessary


### if no raster object but a dimSize is given, the cell size of the raster is
### calculated with the number of cells given by the dimSize NOTE: the dimSize
### is a raw estimate of number of cells of the highest range side. it is
### however not the final number of cells in that direction because when
### calculating the raster it is extended by the ext factor and there is
### rounding with ceiling also taking part.
setMethod(f = "brownian.bridge.dyn", 
          signature = c(object = "SpatialPointsDataFrame", raster = "missing", dimSize = "numeric", location.error = "ANY"), 
          function(object, raster, dimSize, location.error, ...) {
            # print('object SPDF, dimSize numeric')
            if (!any(is.na(bbox))) {
              Range <- extent(bbox)
              Range <- c(Range@xmin, Range@xmax, Range@ymin, Range@ymax)
            } else {
              Range <- .extcalc(obj = object, ext = ext)
            }
            yRange <- diff(Range[3:4])
            xRange <- diff(Range[1:2])
            
            # largest dimension divided by number of cells (=dimSize) gives cell.size
            # (raster='numeric')
            if (xRange > yRange) {
              raster <- xRange/dimSize
            } else {
              raster <- yRange/dimSize
            }
            return(brownian.bridge.dyn(object = object, raster = raster, location.error = location.error, 
                                       margin = margin, window.size = window.size, ext = ext, ...))
          })

# if there is no valid raster object, it should be calculated make a raster
# object and feed it (again) to the brownian.bridge.dyn function (now it will
# call the right function, because raster is now a raster object)
setMethod(f = "brownian.bridge.dyn", 
          signature = c(object = "SpatialPointsDataFrame", raster = "numeric", dimSize = "missing", location.error = "ANY"), 
          definition = function(object, raster, dimSize, location.error, ...) {
            # print('object SPDF, raster numeric')
            if (!any(is.na(bbox))) {
              Range <- extent(bbox)
              Range <- c(Range@xmin, Range@xmax, Range@ymin, Range@ymax)
            } else {
              Range <- .extcalc(obj = object, ext = ext)
            }
            yRange <- diff(Range[3:4])
            xRange <- diff(Range[1:2])
            # calculation of the coordinates to fit squared raster cells
            ymin <- Range[3] - (ceiling(yRange/raster) * raster - yRange)/2
            ymax <- Range[4] + (ceiling(yRange/raster) * raster - yRange)/2
            xmin <- Range[1] - (ceiling(xRange/raster) * raster - xRange)/2
            xmax <- Range[2] + (ceiling(xRange/raster) * raster - xRange)/2
            # Calculate the raster; the raster variable replaces here the cell size
            nrow <- ((ymax - ymin)/raster)
            ncol <- ((xmax - xmin)/raster)
            ex <- extent(c(xmin, xmax, ymin, ymax))
            rst <- raster(ncols = ncol, nrows = nrow, crs = proj4string(object), ex)
            return(brownian.bridge.dyn(object = object, raster = rst, location.error = location.error, margin = margin, window.size = window.size, ext = ext,  ...))
          })

setMethod(f = "brownian.bridge.dyn", 
          signature = c(object = ".MoveTrackSingle", raster = "RasterLayer", dimSize = "missing", location.error = "numeric"), 
          definition = function(object, raster, location.error, ...) {
            if (length(location.error) == 1) location.error <- rep(x = location.error, times = n.locs(object))
            object <- brownian.motion.variance.dyn(object = object, location.error = location.error, margin = margin, window.size = window.size)
            brownian.bridge.dyn(object = object, raster = raster, location.error = location.error, ext = ext,  ...)
          })

setMethod(f = "brownian.bridge.dyn", 
          signature = c(object = "dBMvariance", raster = "RasterLayer", dimSize = "missing", location.error = "numeric"), 
          definition = function(object, raster, location.error,  ext, time.step,...) {
            # check for aeqd projection of the coordinates
            if (grepl("aeqd", proj4string(object)) == FALSE) 
              stop("The projection of the coordinates needs to be \"aeqd\". You may want to use the spTransform funciton to change the projection. \n")
            
            time.lag <- c(time.lag(object, units = "mins"), 0)  #units need to match between here and dBBMMvar calculations
            
            if (missing(time.step)) {
              time.step <- (min(time.lag[-length(time.lag)])/15)
            }
            
            T.Total <- sum(time.lag[object@interest])
            
            compsize <- ncell(raster) * (sum(time.lag[object@interest])/time.step)
            print(paste("Computational size:", sprintf("%.1e", compsize)))
            
            interest <- (c(object@interest, 0) + c(0, object@interest))[1:length(object@interest)] != 0
            # Fortran agguments n.locs gridSize timeDiff total time x track y track
            # variance estimates loc error x raster y raster interpolation time step prop
            # vector filled
            ans <- .Fortran("dBBMM", as.integer(1 + sum(object@interest)), 
                            as.integer(ncell(raster)), 
                            as.double(c(time.lag[object@interest], 0)), 
                            as.double(T.Total), 
                            as.double(coordinates(object)[interest, 1]), 
                            as.double(coordinates(object)[interest, 2]), 
                            as.double(c(object@means[object@interest],0)), 
                            as.double(location.error[interest]), 
                            as.double(coordinates(raster)[, 1]), 
                            as.double(coordinates(raster)[, 2]), 
                            as.double(time.step), as.double(rep(0, ncell(raster))))
            
            raster <- setValues(raster, ans[[12]])
            
            dBBMM <- new("DBBMM", DBMvar = object, method = "Dynamic Brownian Bridge Movement Model", 
                         raster, ext = ext)
            outerProbability <- outerProbability(dBBMM)
            
            if (is.na(outerProbability)) {
              stop("The used extent is too large. Choose a smaller value for ext!")
              # when did this occure Marco? # should we move these checks to the validity
              # function of the dbbbmm object
            } else {
              if (outerProbability > 0.01) {
                warning("outer probability: ", outerProbability, " The used extent is too small. Choose an extent which includes more of the probabilities.")
              }
            }
            
            return(dBBMM)
          })

### do brownian.bridge.dyn for all individuals within a MoveStack
setMethod(f = "brownian.bridge.dyn", 
          signature = c(object = "MoveStack", raster = "RasterLayer", dimSize = "missing", location.error = "numeric"), 
          function(object, raster, dimSize, location.error, ...) {
            # .extcalc already calculated the right raster extension for all tracks
            rm(dimSize)
            ## is not needed anymore, because RasterLayer is already calculated
            moveUnstacked <- split(x = object)
            # split MoveStack into individual Move objects
            dbbmmLST <- list()
            omitMove <- c()
            for (i in names(moveUnstacked)) {
              if (nrow(moveUnstacked[[i]]@coords) > (window.size + margin)) {
                dbbmmLST[[i]] <- brownian.bridge.dyn(moveUnstacked[[i]], raster = raster, location.error = location.error, margin = margin, window.size = window.size, ext = ext, ...)
                # dbbmmLST[[i]] <- dbbmm
              } else {
                omitMove <- c(omitMove, i)
              }
              # store which Move Objects were not processed
            }
            if (length(omitMove) > 0) 
              warning("Move object ", paste(omitMove, collapse = " "), " was/were omitted, because the number of coordinates is smaller than the window.size and margin you use.\n")
            
            # rasterStack <- do.call('stack',dbbmmLST)
            rasterStack <- stack(lapply(dbbmmLST, as, "RasterLayer"))
            DBMvarLST <- lapply(dbbmmLST, slot, "DBMvar")
            objectAnimalsOmitted <- object[as.character(object@trackId) %in% names(DBMvarLST)]
            dBMvarianceStack <- new("dBMvarianceStack", 
                                    objectAnimalsOmitted, 
                                    in.windows = unlist(lapply(DBMvarLST, slot, "in.windows")), 
                                    interest = unlist(lapply(DBMvarLST, slot, "interest")), 
                                    means = unlist(lapply(DBMvarLST, slot, "means")), 
                                    margin = unique(unlist(lapply(DBMvarLST, slot, "margin"))), 
                                    window.size = unique(unlist(lapply(DBMvarLST, slot, "window.size"))))
            # bart break lst should still be inhereted here
            
            browser()
            DBBMMStack <- new("DBBMMStack", DBMvar = dBMvarianceStack, rasterStack)
            return(DBBMMStack)
          })