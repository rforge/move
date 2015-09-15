context('Burst')
test_that('burst',{
  load(system.file("extdata", "move.RData", package="move"))
  b<-burst(leroy, f<-(strftime(timestamps(leroy),'%m')[-1]))
  expect_true(validObject(b))
  expect_equivalent(unlist(lapply(s<-split(b), n.locs))-1, as.numeric(table(f)))
  l<-factor(f);levels(l)<- validNames(levels(l))
  expect_equal(l, burstId(b))
  })