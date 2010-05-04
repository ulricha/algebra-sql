# Author: Jeroen Weijers
# Use this makefile template for compiling lhs2tex projects
# Replace occurences of word Template with the file you want to compile

commonFiles := $(wildcard ../Common/*)
ltxFiles := $(wildcard *.ltx)
fmtFiles := $(wildcard *.fmt)

all : Template.tmp Template.pdf

Template.tmp : Template.pdf
	open ../thesisBin/Thesis.pdf

Template.pdf : ../lit.bib Template.tex  $(commonFiles)
#	svn info | grep 'Revision'          | sed 's/.*\: //' >  ../thesisBin/REVISION
#	svn info | grep 'Last Changed Date' | sed 's/.*(/(/'  >> ../thesisBin/REVISION
	pdflatex -halt-on-error  Template.tex
	bibtex Template
	sh -c ' \
	i=1; \
	while [ $$i -lt 5 ] && ( \
	    grep -c "Rerun" Template.log \
	    || grep -c "undefined citations" Template.log \
	    || grep -c "undefined references" Template.log); \
	  do pdflatex -halt-on-error Template.tex; \
	     i=`expr $$i + 1`; \
	     done; \
	   echo "Iterations: $$i"'
	   open Template.pdf	

Template.tex : $(ltxFiles) $(commonFiles) $(fmtFiles)
	rm -f Template.tex
	lhs2TeX --poly Template.ltx >> Template.tex

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
