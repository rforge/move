context('stored data')
test_that('storedData',{
	names<-load(system.file("extdata", "move.RData", package="move"), .GlobalEnv)
	expect_true( all(unlist(lapply(lapply(names, get), validObject))))
})
