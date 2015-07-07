build:
	GS_QUALITY=ebook R_GSCMD=gs R CMD build --compact-vignettes="both" pkg/move
	
clean:
	rm -rf move*.tar.gz move.Rcheck .Rd2pdf* pkg/move.Rcheck
	svn status --no-ignore | grep '^[I?]' | cut -c 9- | xargs -d"\n" -I{} rm {}


check:
	make build
	time R CMD check --as-cran move*.tar.gz

checkSrc:
	time R CMD check --as-cran pkg/move

checkRdevel:
	/home/bart/bmisc/small_projects/rDevel/svn/R/bin/R CMD check --as-cran move*.tar.gz

changeLog:
	svn2cl -r head:380 --stdout --group-by-day pkg/move/ --strip-prefix='move/' | grep -v '* $$' > pkg/move/ChangeLog

release:
	make clean
	make changeLog
	make build
	make check
	make checkRdevel

man:
	make clean
	R CMD Rd2pdf pkg/move/
