setClass ('MoveA', contains = c('Move', 'VIRTUAL') )

setClass(Class="MoveStack",
	 contains="MoveA",
	 representation(
			animala="vector",
			speciesa="vector")
)


if (!isGeneric("moveStack")) {setGeneric("moveStack", function(x, ...) standardGeneric("moveStack"))}

##Creating a stacked move object
setMethod(f="moveStack", 
          signature=c(x="data.frame"), 
          definition = function(x){
            return(res)##MoveStack object
          }
          )