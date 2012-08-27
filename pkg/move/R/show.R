### Show Method for the data object Move
setMethod("show", "Move", function(object){
  print(object)
})

setMethod("show", "MoveStack", function(object){
  print(object) 
})
