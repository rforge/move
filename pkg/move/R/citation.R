setGeneric("citation", function(obj) standardGeneric("citation"))
setMethod("citation", ".MoveGeneral", function(obj){
  return(obj@citation)
})

setGeneric("citation<-", function(obj, value) standardGeneric("citation<-"))
setReplaceMethod("citation", ".MoveGeneral", function(obj, value){
  if (length(value)!=1) {
    value <- as.character(value[1])
    warning("There were more than one citation entered. Only using the first element!")
  }
  obj@citation <- value
  obj
})
