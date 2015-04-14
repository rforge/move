context('spTransform')
test_that('spTransfrom',{
	data <- move(file<-system.file("extdata","leroy.csv.gz",package="move"))
	data2<-spTransform(x=d<-spTransform(data,center=T), CRSobj=(proj4string(data)))
	data@coords.nrs=numeric(0)# sptransfrom does this for some reason in line 123 of project.R
	expect_equal(data,data2)
	expect_error(spTransform(d, center=T))# somethign not long lat cant go to aeqd
})
