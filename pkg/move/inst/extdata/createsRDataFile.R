##creates the 
require(move)
leroy <- move(system.file("extdata","leroy.csv", package="move"))
ricky <- move(system.file("extdata","ricky.csv", package="move"))[, ]
stack <- moveStack(list(leroy, ricky))
leroydbbmm <- brownian.bridge.dyn(spTransform(leroy), dimSize=85, location.error=23, time.step=5)
rickydbbmm <- brownian.bridge.dyn(spTransform(ricky), dimSize=65, location.error=23, time.step=5)
dbbmmstack <- brownian.bridge.dyn(spTransform(stack), dimSize=85, location.error=23, ext=.3, time.step=600)
save(leroy, ricky, stack, leroydbbmm, rickydbbmm, dbbmmstack, file="move.RData")

