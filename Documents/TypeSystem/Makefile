# Author: Jeroen Weijers
# Use this makefile template for compiling lhs2tex projects
# Replace occurences of word TypeSystem with the file you want to compile

commonFiles := $(wildcard ../Common/*)
ltxFiles := $(wildcard *.ltx)
fmtFiles := $(wildcard *.fmt)

all : TypeSystem.pdf

TypeSystem.pdf : ../lit.bib TypeSystem.tex  $(commonFiles)
#	svn info | grep 'Revision'          | sed 's/.*\: //' >  ../thesisBin/REVISION
#	svn info | grep 'Last Changed Date' | sed 's/.*(/(/'  >> ../thesisBin/REVISION
	pdflatex -halt-on-error  TypeSystem.tex
	# bibtex TypeSystem
	sh -c ' \
	i=1; \
	while [ $$i -lt 5 ] && ( \
	    grep -c "Rerun" TypeSystem.log \
	    || grep -c "undefined citations" TypeSystem.log \
	    || grep -c "undefined references" TypeSystem.log); \
	  do pdflatex -halt-on-error TypeSystem.tex; \
	     i=`expr $$i + 1`; \
	     done; \
	   echo "Iterations: $$i"'
	   open TypeSystem.pdf	

TypeSystem.tex : $(ltxFiles) $(commonFiles) $(fmtFiles)
	rm -f TypeSystem.tex
	lhs2TeX --poly TypeSystem.ltx >> TypeSystem.tex

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
