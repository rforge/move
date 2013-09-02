
test.subSet<-function()
{
	a<-move(x=1:10,y=1:10,time=as.POSIXct(1:10, origin='1970-1-1'),proj=CRS('+proj=longlat'))
	b<-move(x=1:10,y=1:10,time=as.POSIXct(1:10, origin='1970-1-1'),proj=CRS('+proj=longlat'),animal='b') 
	s<-moveStack(list(a,j=b))
	sl<-moveStack(list(l=a,j=b, k=a,p=b ))
	bb<-s[['j']]
	names(bb@sensor)<-NULL
	names(bb@timestamps)<-NULL
	rownames(bb@data)<-(1:10)
	rownames(bb@coords)<-NULL
	bb@idData<-b@idData
	#need to make sure the results are more equal
	attr(b@timestamps,"tzone")<-NULL
	checkEquals(b, bb)
	checkEquals(s[['j']], split(s)[['j']]) 
	checkEquals(s[[1]], s[[c(T,F)]])
	checkEquals(s[[2]], s[['j']])
	ss<-s[[T]]
#	ss@bbox<-bbox(s)
#	attributes(ss@coords)$dimnames[[2]]<- attributes(s@coords)$dimnames[[2]]
	checkEquals(s,ss)
	checkEquals(sl[[idData(sl,T,'individual.local.identifier')=='b']], sl[[c(2,4)]])
	checkEquals(sl[[c('j','p')]], sl[[c(2,4)]])
#	for(i in slotNames(b))
#	{
#		message(i)
#		try(checkEquals(slot(b,i), slot(bb,i)))
#	}
	

}
