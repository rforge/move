setClass(Class = ".UDStack", contains = c("RasterStack"), 
         representation = representation(method = "character"), 
         prototype = prototype(
           method = as.character()), 
         validity = function(object) {
        if (!all(apply(values(object), MARGIN = 2, FUN = function(X) isTRUE(all.equal(sum(X), 1, check.attributes=F))))) 
            stop("One or more of the used rasters are not a UD (sum is not equal to 1)")
    })

setClass(Class = ".UD", contains = c("RasterLayer"), 
         representation = representation(method = "character"), 
         prototype = prototype(
           method = as.character()), 
         validity = function(object) {
        if (!isTRUE(all.equal(tmp<-sum(values((object))), 1))) 
            stop("The used raster is not a UD (sum unequal to 1), sum is: ", sprintf("%.15f",tmp))
        return(TRUE)
    })

### Defining the class of the Brownian Bridge Movement Model object
setClass(Class = "DBBMMStack", contains = c(".UDStack"), 
         representation = representation(
           DBMvar = "dBMvarianceStack", 
           ext = "numeric"), 
         prototype = prototype(
           ext = as.numeric()), 
         validity = function(object) {
    if (!all(unique(object@DBMvar@trackId) == object@layernames)) 
        stop("The layer names of the raster objects do not match the trackIDs of the DBMvarStack.")
})

setClass(Class = "DBBMM", contains = c(".UD"), 
         representation = representation(
           DBMvar = "dBMvariance", 
           ext = "numeric"), 
         prototype = prototype(ext = as.numeric()))

# See if this validity check needs to go into this class why was it commented ?
# validity = function(object){ if(is.na(outerProbability(object))){ stop('The
# used extent is too large. Choose a smaller value for ext!')# when did this
# occure Marco?  } else { if (outerProbability > .01){ warning('outer
# probability: ', outerProbability,' The used extent is too small. Choose an
# extent which includes more of the probabilities.') } } }
