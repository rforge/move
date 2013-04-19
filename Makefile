build:
	GS_QUALITY=ebook R_GSCMD=gs R CMD build --compact-vignettes="both" pkg/move
	
clean:
	move*.tar.gz

check:
	R CMD check --as-cran move*.tar.gz

