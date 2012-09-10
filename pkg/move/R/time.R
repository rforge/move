setGeneric("timeSummary", function(x){standardGeneric("timeSummary")})
setMethod("timeSummary", 
          signature=".MoveTrackSingle",
          definition=function(x){           
            date <- timestamps(x)
            TimeDiff <- time.lag(x, units="mins") #time differences in minutes
            #             out <- boxplot(seglength(x)/(as.numeric(TimeDiff+0.0000001)), range=10, plot=F)$out ##marco there must be an easier way than this
            #             if(length(out)!=0) x@idData$out <- out else x@idData$out <- NA
            #             x@idData$outliners <- length(out[out > median(out)])>0   #check whether there are strong outliers in speed
            df <- data.frame(Duration=difftime(date[length(date)], date[1], units="hours")) #total duration of the track in hours
            df$AverDur <- mean(TimeDiff)       #mean time difference between relocations
            df$SDDur <- sd(TimeDiff)           #standard deviation of time differences between relocations
            df$dupl <- any((TimeDiff)<(1/3600))            #check whether any two relocations are closer than a second to each other
            df$multseason <-  any(TimeDiff > (24*30))      #check whether any two subsequent relocations are more than one month apart
            return(df)
          })

setMethod("timeSummary", 
          signature=".MoveTrackStack", 
          definition=function(x){
            lst <- lapply(split(x), timeSummary)
            return(lst)
          })



# setGeneric("time")#, function(x){standardGeneric("time")})
# setMethod("time", 
#           signature=".MoveTrackSingle",
#           definition=function(x){           
#             date <- timestamps(x)
#             time.lag(x) #time differences in minutes
#           })
# 
# setMethod("time", 
#           signature=".MoveTrackStack", 
#           definition=function(x){
#             lst <- lapply(split(x), time)
#             return(lst)
#           })


