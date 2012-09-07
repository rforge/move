
test.citations<-function(){
	a<-move(x=1:10,y=1:10,time=as.POSIXct(1:10, origin='1970-1-1'),proj=CRS('+proj=longlat'))
	checkEquals(citations(a), character())
	citations(a)<-"bla"
	checkEquals(citations(a),"bla")
	checkException(citations(a)<-factor("a"))
	tmp<-options(warn=2)$warn
	checkException(citations(a)<-c("a","a"))
	options(warn=tmp)
}
	
