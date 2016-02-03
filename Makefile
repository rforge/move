build:
	GS_QUALITY=ebook R_GSCMD=gs R CMD build --compact-vignettes="both" pkg/move
	
clean:
	rm -rf move*.tar.gz move.Rcheck .Rd2pdf* pkg/move.Rcheck
	#svn status --no-ignore | grep '^[I?]' | cut -c 9- | xargs -d"\n" -I{} rm {} # fix to not remove project


check:
	make build
	time R CMD check --as-cran move*.tar.gz

checkSrc:
	time R CMD check --as-cran pkg/move

dockerSetup:
	docker pull rocker/r-devel
	docker --dns="8.8.8.8" build -t bart/rdevel .
	# case of network problems try restarting docker sudo service docker restart

checkRdevel:
	cp `ls move*.tar.gz | sort -V | tail -n 1`  dockerOutput/move.tar.gz
	docker run -it -v /home/bart/bmisc/small_projects/move/dockerOutput:/tmp bart/rdevel Rdevel CMD check --as-cran /tmp/move.tar.gz -o /tmp

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
