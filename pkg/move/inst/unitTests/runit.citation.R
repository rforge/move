
test.citation<-function(){
	a<-move(x=1:10,y=1:10,time=as.POSIXct(1:10, origin='1970-1-1'),proj=CRS('+proj=longlat'))
	checkEquals(citation(a), character())
	citation(a)<-"bla"
	checkEquals(citation(a),"bla")
	checkException(citation(a)<-factor("a"))
	tmp<-options(warn=2)$warn
	checkException(citation(a)<-c("a","a"))
	options(warn=tmp)
}
	
