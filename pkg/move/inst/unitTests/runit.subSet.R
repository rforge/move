
test.moveStack<-function()
{
	a<-move(x=1:10,y=1:10,time=as.POSIXct(1:10, origin='1970-1-1'),proj=CRS('+proj=longlat'))
	b<-move(x=1:10,y=1:10,time=as.POSIXct(1:10, origin='1970-1-1'),proj=CRS('+proj=longlat'),animal='b') 
	s<-moveStack(list(a,j=b))
	bb<-s[['j']]
	names(bb@sensor)<-NULL
	names(bb@timestamps)<-NULL
	rownames(bb@data)<-(1:10)
	bb@idData<-b@idData
	#need to make sure the results are more equal
	checkEquals(b, bb)

}
