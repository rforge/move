test.moveStack<-function()
{
	a<-move(x=1:10,y=1:10,time=as.POSIXct(1:10, origin='1970-1-1'),proj=CRS('+proj=longlat'))
	b<-move(x=1:10,y=1:10,time=as.POSIXct(1:10, origin='1970-1-1'),proj=CRS('+proj=longlat'), 
		animal="a")
	checkIdentical(coordinates(a), coordinates(b))
	DEACTIVATED("Need to look what we want here")
	bb<-split(moveStack(list(a,b)))
	aa<-list(unnamed=a,a=b)
	row.names(aa[[2]])<-1:10
	row.names(bb[[2]])<-1:10
	checkEquals(bb,aa)# one problem seems to be moveStack does not deal with missed fixes, the other the rownames of the data frame

}
