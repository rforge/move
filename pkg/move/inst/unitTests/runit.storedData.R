test.storedData<-function(){
	names<-load(system.file("extdata", "move.RData", package="move"), .GlobalEnv)
	checkTrue( all(unlist(lapply(lapply(names, get), validObject))))
}
