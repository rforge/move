setClass ('MoveA', contains = c('Move', 'VIRTUAL') )

setClass(Class="MoveStack",
	 contains="MoveA",
	 representation(
			animala="vector",
			speciesa="vector")
)
