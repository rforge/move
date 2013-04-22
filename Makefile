build:
	GS_QUALITY=ebook R_GSCMD=gs R CMD build --compact-vignettes="both" pkg/move
	
clean:
	rm -rf move*.tar.gz move.Rcheck

check:
	R CMD check --as-cran move*.tar.gz

cl:
	svn2cl -r head:380 --stdout --group-by-day pkg/move/ --strip-prefix='move/' | grep -v '* $$' > pkg/move/ChangeLog

release:
	make clean
	make cl
	make build
	make check
