# Author: Jeroen Weijers
# Use this makefile template for compiling lhs2tex projects
# Replace occurences of word FerryCore with the file you want to compile

commonFiles := $(wildcard ../Common/*)
ltxFiles := $(wildcard *.ltx)
fmtFiles := $(wildcard *.fmt)

all : FerryCore.pdf


FerryCore.pdf : ../lit.bib FerryCore.tex  $(commonFiles)
#	svn info | grep 'Revision'          | sed 's/.*\: //' >  ../thesisBin/REVISION
#	svn info | grep 'Last Changed Date' | sed 's/.*(/(/'  >> ../thesisBin/REVISION
	pdflatex -halt-on-error  FerryCore.tex
#	bibtex FerryCore
#	sh -c ' \
#	i=1; \
#	while [ $$i -lt 5 ] && ( \
#	    grep -c "Rerun" FerryCore.log \
#	    || grep -c "undefined citations" FerryCore.log \
#	    || grep -c "undefined references" FerryCore.log); \
#	  do pdflatex -halt-on-error FerryCore.tex; \
#	     i=`expr $$i + 1`; \
#	     done; \
#	   echo "Iterations: $$i"'
	   open FerryCore.pdf	

FerryCore.tex : $(ltxFiles) $(commonFiles) $(fmtFiles)
	rm -f FerryCore.tex
	lhs2TeX --poly FerryCore.ltx >> FerryCore.tex

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
