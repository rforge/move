test.moveStack<-function()
{
	a<-move(x=1:10,y=1:10,time=as.POSIXct(1:10, origin='1970-1-1'),proj=CRS('+proj=longlat'))
	b<-move(x=1:10,y=1:10,time=as.POSIXct(1:10, origin='1970-1-1'),proj=CRS('+proj=longlat'), 
		animal="a")
	checkIdentical(coordinates(a), coordinates(b))
#	DEACTIVATED("Need to look what we want here")
	bb<-split(d<-moveStack(list(a,b)))
	aa<-list(unnamed=a,a=b)
	row.names(aa[[2]])<-1:10
	row.names(bb[[2]])<-1:10
	bb<-lapply(bb, function(x){x@idData$individual.local.identifier<-factor(x@idData$individual.local.identifier); return(x)})
	checkEquals(bb,aa)# one problem seems to be moveStack does not deal with missed fixes, the other the rownames of the data frame
	row.names(d@idData)<-sub('a','A A', row.names(d@idData))
	checkException(new('MoveStack', d , trackId=factor(sub('a','A A', as.character(d@trackId))), idData=d@idData))# track ids are no good names
  checkException(validObject(d))#validity check needs to fail because of changed rownames
  
	a<-move(x=1:10,y=1:10,time=as.POSIXct(1:10, origin='1970-1-1'),proj=CRS('+proj=longlat'),animal="AAA")
	a2<-move(x=1:10,y=1:10,time=as.POSIXct(1:10, origin='1970-1-1'),proj=CRS('+proj=longlat'),animal="AAA")  
	tmp<-options(warn=2)$warn
	checkException(moveStack(list(a,a2)))# warn about duplicate ids
	options(warn=tmp)
}
