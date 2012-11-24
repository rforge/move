setGeneric("moveStack", function(x) standardGeneric("moveStack"))#marco what is proj here?
setMethod(f = "moveStack", 
          signature = c(x="list"),
          definition = function(x){
            if (any((as.character(lapply(x, class)))!="Move")) 
              stop("One or more objects in the list are not from class Move")
            if(!all(unlist(lapply(x, validObject))))
              stop("Not all valid moveobjects")
            if (length(unique(as.character(lapply(x, function(y) attr(slot(y, "timestamps"), "tzone")) )))!=1)
              stop("One or more objects in the list have no UTC timestamps")
            
            proj <- lapply(lapply(x, proj4string), strsplit, split=" ")
            #if (length(unique(as.character(lapply(x,proj4string))))!=1)
            #if(!all(unlist(lapply(proj, identical, y=proj[[1]]))))    
#             projs <- unlist(lapply(lapply(lapply(proj, unlist), grepl, pattern=unlist(proj[[1]])[2]), any))
#             if(!all(is.na(projs)) & !all(duplicated(projs)[-1]))
#             browser()
            try(projs<-length(unique(do.call(rbind,lapply(proj, unlist))[,2]))!=1, TRUE) #check all euql projections
            if(!exists("projs")) try(projs<-length(unique(do.call(rbind,lapply(proj, unlist))))!=1, TRUE) #check all projections == NA
            if(projs)
              stop("One or more objects in the list have differnt projections. All projections have to be the same")
            if(any(duplicated(unlist(lapply(lapply(x, slot, "idData"), rownames))))){
              nnames <- make.names(unlist(lapply(lapply(x, slot, "idData"), rownames)),unique=T)
              lapply(1:length(nnames), function(z, nnames, x) {rownames(x[[z]]@idData)<-nnames[z]
                                                               return(x[[z]])}, x=x, nnames=nnames)
              warning("Detected duplicated names. Renamed the duplicated individuals accordingly.")
              #check: lapply(split(moveStack(list(data,data))), slot, 'idData')
            }
            length <- lapply(lapply(x, coordinates), nrow)
            coords <- do.call(rbind, lapply(x, coordinates))
            colnames(coords) <- c("location.long", "location.lat")
            allData <- lapply(x, function(y) slot(y, "data"))
            allColumns <- unique(unlist(sapply(allData, names)))
            
            ###DATA
            DATA <- do.call("rbind", lapply(allData, FUN = function(entry) {
              missingColumns <- allColumns[which(!allColumns %in% names(entry))]
              entry[, missingColumns] <- NA
              entry})) #thanks to: Karl Ove Hufthammer
            
            ###idData
            allidData <- lapply(x, function(y) slot(y, "idData"))
            allidColumns <- unique(unlist(sapply(allidData, names)))
            IDDATA <- do.call("rbind", lapply(allidData, FUN = function(entry) {
              missingColumns <- allidColumns[which(!allidColumns %in% names(entry))]
              entry[, missingColumns] <- NA
              entry}))
            id <- rownames(IDDATA)
            
            spdftmp <- SpatialPointsDataFrame(
              coords = coords,
              data = DATA, 
              proj4string = CRS(proj4string(x[[1]])), #projection tested above
              match.ID = TRUE)
	    tmf<-lapply(x, slot,"timesMissedFixes")
	    tz<-unique(unlist(lapply(tmf, attr, "tzone")))
	    if(!(length(tz)==1|is.null(tz )))
		    stop("Concatinating multiple time zone for timest missed fixes")
	    tmfVector<-do.call('c',lapply(1:length(tmf), function(i, tmf, names){tmp<-tmf[[i]]; if(!is.null(tmp)) names(tmp)<-rep(names[i], length(tmp)); return(tmp)}, tmf=tmf, names=rownames(IDDATA)))
	    if(!is.null(tz))
	     tmfVector<-as.POSIXct(format(tmfVector, tz=tz,usetz=T),tz=tz)
            res <- new("MoveStack", 
		       timesMissedFixes=tmfVector,
                       idData = IDDATA,
                       spdftmp, 
                       timestamps = do.call("c", lapply(x, timestamps)),
                       #as.POSIXct(do.call(rbind, (lapply(x, function(y) {as.data.frame(y@timestamps)})))[,1], tz="UTC"), #timezone?
                       sensor =factor(do.call('c',lapply(lapply(x, slot, 'sensor'),as.character))),# do.call("factor", (lapply(x, slot, "sensor"))), 
                       trackId = as.factor(rep(id, length)))
            return(res)
          })

