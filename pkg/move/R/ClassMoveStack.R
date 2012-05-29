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
            for (i in length(unique(individualID))){
              create a move object
              and store it in MoveStack Object under animalName ...
            }
            
            return(res)##MoveStack object
          }
          )