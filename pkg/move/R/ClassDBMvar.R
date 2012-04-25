###Defining the class of the Dynamic Brownian Motion Variance object
require(sp)
setClass(Class = "DBMvar",
         representation = representation (
           means      = "array", 
           in.windows = "array",
           interest   = "logical",
           break.list = "ANY")
         )

## Making dBMvar a generic funtion
#if (!isGeneric("dBMvar")) {  
setGeneric("dBMvar", function(BMvars, BMvar, n.locs, break.list) standardGeneric("dBMvar"))
#} 

## Defining the funcitoin dBMvar
setMethod(f="dBMvar", 
          signature = c(BMvars = "data.frame", BMvar ="list", n.locs = "numeric", break.list = "ANY"), 
          definition = function(BMvars, BMvar, n.locs, break.list){
            res <- new(Class="DBMvar")
            res@means <- tapply(BMvars$BMvar, BMvars$loc, "mean")
            res@in.windows <- tapply(BMvars$BMvar, BMvars$loc, "length")
            res@interest <- 1:n.locs %in% as.numeric(names(res@in.windows[res@in.windows == max(res@in.windows)]))
            res@break.list <- break.list
            return(res)
            }
          )


#if (!isGeneric("brownian.motion.variance.dyn")) {
  setGeneric("brownian.motion.variance.dyn", function(object, location.error, window.size, margin){standardGeneric("brownian.motion.variance.dyn")})
#}

setMethod(f= "brownian.motion.variance.dyn",
          signature = c(object="Move",location.error="numeric",window.size="numeric",margin="numeric"),
          definition = function(object, location.error, window.size, margin){
            
            time.lag <- time.lag(object)
            x <- coordinates(object)[ ,1]
            y <- coordinates(object)[ ,2]
            n.locs <- n.locs(object)
            
        if(any((c(margin, window.size)%%2)!=1)){
          stop("Margin and window size need to be uneven")
        } else {}
        
            brownian.motion.variance <- function(n.locs, time.lag, location.error, x, y){
              # function to calculate brownian.motion.variance for a piece of track
          		#Creating NULL vectors to store data
          		T.jump <- alpha <- ztz <- loc.error.1 <- loc.error.2 <- NULL
          		i <- 2
          		while(i < n.locs){#calculate serveral variables needed in the variance function
          			t <- time.lag[i-1] + time.lag[i]
          			T.jump <- c(T.jump, t)
          			a <- time.lag[i-1] / t
          			alpha <- c(alpha, a)
          			u <- c(x[i-1], y[i-1]) + a*(c(x[i+1], y[i+1]) - c(x[i-1], y[i-1]))
          			ztz <- c(ztz, (c(x[i], y[i]) - u)%*%(c(x[i], y[i]) - u))
          			loc.error.1 <- c(loc.error.1, location.error[i-1])
          			loc.error.2 <- c(loc.error.2, location.error[i+1])
          			i <- i + 2
          		}
          		#Likelihood function for Brownian Motion variance estimation
          		likelihood <- function(var, T.jump, alpha, loc.error.1, loc.error.2, ztz){   
          			v <- T.jump*alpha*(1-alpha)*var + ((1-alpha)^2)*(loc.error.1^2) + (alpha^2)*(loc.error.2^2)
          			l <- (1/(2*pi*v))*exp(-ztz/(2*v)) 
          			return(-sum(log(l), na.rm=TRUE))# bart check na.rm=T
          		}
          		BMvar <- optimize(likelihood, lower=(l<-0), upper=(u<-1000000000000000), T.jump=T.jump, alpha=alpha, loc.error.1=loc.error.1, loc.error.2=loc.error.2, ztz=ztz)# implement checks if optimization worked
			if(BMvar$minimum %in% c(l,u))
			{
				stop("optimization failed maybe consider changing mapunits")
			}
          		if((length(x)%%2)!=1){
          			warning("Not an even number of location in variance function")
          		}
          		return(list(BMvar=BMvar$minimum, cll=-BMvar$objective))
              }
            
        #print("brownian.motion.variance.dyn 1")
        
        BMvar <- list()
        window.starts <- 1:(length(x)-window.size+1)
        breaks <- margin:(window.size-margin+1)
        break.list <- c()
        uneven.breaks <- breaks[(breaks%%2)==1]
        BMvars <- data.frame(BMvar=c(), loc=c())
        
        #print("brownian.motion.variance.dyn 2")
        
        if(length(breaks)<2){
         stop("Margin to window ratio not ok") #What would be necessary to change?
         } 
    
        
        for(w in window.starts)# try all windows in tracke
        {
              #calculate all vectors for parts inside the window
              x.sub <- x[w:(w-1+window.size)]
              y.sub <- y[w:(w-1+window.size)]
              time.lag.sub <- time.lag[w:(w-1+window.size)]
              location.error.sub <- location.error[w:(w-1+window.size)]
    	  # calculate bmvar for whole window
              whole <- brownian.motion.variance(n.locs=window.size, x=x.sub, y=y.sub, location.error=location.error.sub, time.lag=time.lag.sub) 
              whole$BIC <- -2*whole$cll+log(window.size)
              brk.res <- list(BIC=Inf) ##is a list use inf so all other numbers are lower
              
              for(b in uneven.breaks)# try all possible breaks and find the one with minimal BIC, b represents break location
              {
                before <- brownian.motion.variance(n.locs=b, x=x.sub[1:b], y=y.sub[1:b], location.error=location.error.sub[1:b], time.lag=time.lag.sub[1:b])
                after <- brownian.motion.variance(n.locs=window.size-b+1, x=x.sub[b:window.size], y=y.sub[b:window.size], location.error=location.error.sub[b:window.size], time.lag=time.lag.sub[b:window.size])
                #% minimize aic check N (window.size) here TODO
                #% tmp <- c(tmp,-2*(before$cll+after$cll)+2*log(window.size))
                #%			print(before$cll+after$cll)
    	    brkBIC<-(-2*(before$cll+after$cll)+2*log(window.size))
                if(brkBIC<brk.res$BIC)
    	        {
                  brk.res <- list( w=w, b=b, var.before=before$BMvar, var.after=after$BMvar, BIC=brkBIC )
    	        }
              }#brk.res should now be the best possible break
                      
              windowBMvar <- rep( whole$BMvar, length(breaks)-1 )
              
              if(brk.res$BIC<whole$BIC){# check if the break is better than any other, if so use those variance values
                windowBMvar <- c(rep(brk.res$var.before, sum(breaks<brk.res$b)),  rep(brk.res$var.after, sum(breaks>brk.res$b)))
                break.list <- c(break.list, (w-1+brk.res$b))
              } else{}
              BMvars <- rbind(BMvars, data.frame(BMvar=windowBMvar, loc=w-1+margin:(window.size-margin)	))
        }
        
        
        #print("brownian.motion.variance.dyn 3")
        
        #CALL IT DBMvar
        DBMvar <- dBMvar(BMvars=BMvars, BMvar=BMvar, n.locs=n.locs, break.list=break.list) 
        
        #print("brownian.motion.variance.dyn 4")
                
        i <- DBMvar@interest
        if((sum(i)%%2)==0){    # if even one more location can be included in bb calculations
          i <- (c(DBMvar@interest,0)+c(0,DBMvar@interest))[1:length(DBMvar@interest)]!=0
        }
        
        return(DBMvar)
  }
)


