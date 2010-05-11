# Author: Jeroen Weijers
# Use this makefile template for compiling lhs2tex projects
# Replace occurences of word NormaliseDoc with the file you want to compile

commonFiles := $(wildcard ../Common/*)
ltxFiles := $(wildcard *.ltx)
fmtFiles := $(wildcard *.fmt)

all : NormaliseDoc.tmp NormaliseDoc.pdf

NormaliseDoc.tmp : NormaliseDoc.pdf
	open NormaliseDoc.pdf

NormaliseDoc.pdf : ../lit.bib NormaliseDoc.tex  $(commonFiles)
#	svn info | grep 'Revision'          | sed 's/.*\: //' >  ../thesisBin/REVISION
#	svn info | grep 'Last Changed Date' | sed 's/.*(/(/'  >> ../thesisBin/REVISION
	pdflatex -halt-on-error  NormaliseDoc.tex
#	bibtex NormaliseDoc
#	sh -c ' \
#	i=1; \
#	while [ $$i -lt 5 ] && ( \
#	    grep -c "Rerun" NormaliseDoc.log \
#	    || grep -c "undefined citations" NormaliseDoc.log \
#	    || grep -c "undefined references" NormaliseDoc.log); \
#	  do pdflatex -halt-on-error NormaliseDoc.tex; \
#	     i=`expr $$i + 1`; \
#	     done; \
#	   echo "Iterations: $$i"'
	   open NormaliseDoc.pdf	

NormaliseDoc.tex : $(ltxFiles) $(commonFiles) $(fmtFiles)
	rm -f NormaliseDoc.tex
	lhs2TeX --poly NormaliseDoc.ltx >> NormaliseDoc.tex

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
