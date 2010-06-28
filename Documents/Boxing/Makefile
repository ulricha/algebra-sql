# Author: Jeroen Weijers
# Use this makefile template for compiling lhs2tex projects
# Replace occurences of word Template with the file you want to compile

commonFiles := $(wildcard ../Common/*)
ltxFiles := $(wildcard *.ltx)
fmtFiles := $(wildcard *.fmt)

all : Template.tmp Template.pdf

Template.tmp : Boxing.pdf
	open ../Boxing.pdf

Boxing.pdf : ../lit.bib Boxing.tex  $(commonFiles)
#	svn info | grep 'Revision'          | sed 's/.*\: //' >  ../thesisBin/REVISION
#	svn info | grep 'Last Changed Date' | sed 's/.*(/(/'  >> ../thesisBin/REVISION
	pdflatex -halt-on-error  Boxing.tex
#	bibtex Boxing
#	sh -c ' \
#	i=1; \
#	while [ $$i -lt 5 ] && ( \
#	    grep -c "Rerun" Boxing.log \
#	    || grep -c "undefined citations" Boxing.log \
#	    || grep -c "undefined references" Boxing.log); \
#	  do pdflatex -halt-on-error Boxing.tex; \
#	     i=`expr $$i + 1`; \
#	     done; \
#	   echo "Iterations: $$i"'
	   open Boxing.pdf	

Boxing.tex : $(ltxFiles) $(commonFiles) $(fmtFiles)
	rm -f Boxing.tex
	lhs2TeX --poly Boxing.ltx >> Boxing.tex

clean :
	sh -c 'rm -f *.aux'
	sh -c 'rm -f *.bbl'
	sh -c 'rm -f *.blg'
	sh -c 'rm -f *.log'
	sh -c 'rm -f *.pdf'
	sh -c 'rm -f *.gz'
	sh -c 'rm -f *.toc'
	sh -c 'rm -f *.tex'
	sh -c 'rm -f *.out'
	sh -c 'rm -f *.ptb'
	sh -c 'rm -f REVISION'

.PHONY : clean all
