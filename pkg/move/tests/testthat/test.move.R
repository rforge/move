context('move')
test_that('move',
{
	expect_error(move())
	#expect_error(move(x=1:10, y=1:10, time=as.POSIXct(1:10, origin="1970-1-1"))) #we now allow projection to be NA
	 data <- move(file<-system.file("extdata","leroy.csv.gz",package="move"))
	cvs<-na.omit(read.csv(file)[,c('location.long','location.lat')])
	expect_equivalent(coordinates(data),as.matrix(cvs))
	data2<-move(file(file))
	data@dateCreation<- data2@dateCreation
	expect_equal(data, data2)
	expect_message(data3<-move(file(file), removeDuplicatedTimestamps=T),'removeDupilcatedTimestamps was set to true we strongly suggest against it and that the problem is solved before because there is no systematic to which locations are removed. This can for example be done by marking them outlier in movebank'
	)
	data3@dateCreation<- data2@dateCreation
	expect_equal(data, data3)
	tmp<-options(warn=2)$warn
	expect_error( suppressMessages(data3<-move(file(file), removeDuplicatedTimestamps=T)))
	options(warn=tmp)
fileR<-system.file("extdata","ricky.csv.gz",package="move")
if(class(try(move(pipe(paste0('zcat ',fileR, " | sed ''")))))=='Move'){
	dataR<-move(pipe(paste0('zcat ',fileR)))
	expect_error(move(pipe(paste0('zcat ',fileR,"| sed 'p;'"))))
	tmp<-options(warn=2)$warn
	expect_error(move(pipe(paste0('zcat ',fileR,"| sed '1!p;'"))))
	options(warn=tmp)
	dataR2<-(move(pipe(paste0('zcat ',fileR,"| sed '1!p;'"))))
	dataR@dateCreation<- dataR2@dateCreation
	rownames(dataR@data)<- as.character(rownames(dataR2@data))
	rownames(dataR2@data)<- as.character(rownames(dataR2@data))
	rownames(dataR@dataUnUsedRecords)<- as.character(rownames(dataR2@dataUnUsedRecords))
	rownames(dataR2@dataUnUsedRecords)<- as.character(rownames(dataR2@dataUnUsedRecords))
	rownames(dataR2@coords)<- rownames(dataR@coords)
	expect_equal(dataR, dataR2)
	expect_error(dataR3<-(move(pipe(paste0('zcat ',fileR,"| sed '3p; 3s/Martes/Maggg/'")))))
	expect_error(move(pipe(paste0('zcat ',fileR,"| sed '3p; 3s/\"A\"/\"B\"/'")), removeDuplicatedTimestamps=F))
	expect_error(move(pipe(paste0('zcat ',fileR,"| sed '4p; 4s/9025698/9023698/'")), removeDuplicatedTimestamps=F))
	expect_equal(n.locs(move(pipe(paste0('zcat ',fileR,"| sed '5p; 5s/28.999/29.000/'")), removeDuplicatedTimestamps=F)), n.locs(dataR2)+1)
	suppressMessages(dataR3<-move(pipe(paste0('zcat ',fileR,"| sed '3p; 3s/292.95/293.95/'")), removeDuplicatedTimestamps=T))
	dataR@dateCreation<- dataR3@dateCreation
	rownames(dataR@data)<- as.character(rownames(dataR3@data))
	rownames(dataR3@data)<- as.character(rownames(dataR3@data))
	rownames(dataR@dataUnUsedRecords)<- as.character(rownames(dataR3@dataUnUsedRecords))
	rownames(dataR3@dataUnUsedRecords)<- as.character(rownames(dataR3@dataUnUsedRecords))
  rownames(dataR3@coords)<- rownames(dataR@coords)
	expect_equal(dataR, dataR3)
}
}
)