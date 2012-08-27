# Defining the class of the Dynamic Brownian Motion Variance object
# setClass(Class = 'DBMvar', representation = representation ( means = 'array',
# in.windows = 'array', interest = 'logical', break.list = 'ANY') )


setClass(Class = "dBMvarianceTmp", 
         representation = representation(
           window.size = "numeric", 
           margin = "numeric", 
           means = "numeric", 
           in.windows = "numeric", 
           interest = "logical", 
           break.list = "numeric"), 
         prototype = prototype(
           window.size = numeric(), 
           margin = numeric(), 
           means = numeric(), 
           in.windows = numeric(), 
           interest = logical(), 
           break.list = numeric()), 
    validity = function(object) {
        if (length(unique(c(length(object@means), length(object@in.windows), length(object@interest)))) != 1) 
            stop("Length does not match")
        if (length(object@margin) != 1) 
            stop("Margin length not 1")
        if (length(object@window.size) != 1) 
            stop("Window size length not 1")
        return(TRUE)
    })

setClass(Class = "dBMvariance", contains = c(".MoveTrackSingle", "dBMvarianceTmp"), 
    validity = function(object) {
        if (length(object@means) != nrow(object@coords)) 
            stop("Number of coordinates does not match the number of means")
        return(TRUE)
    })


setClass(Class = "dBMvarianceStack", contains = c(".MoveTrackStack", "dBMvarianceTmp"), 
    validity = function(object) {
        if (length(object@means) != nrow(object@coords)) 
            stop("Number of coordinates does not match the number of means")
        return(TRUE)
    })
