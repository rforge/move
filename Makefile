build:
	R CMD build --compact-vignettes pkg/move
	
clean:
	move*.tar.gz

check:
	R CMD check --as-cran move*.tar.gz

