# Author: Jeroen Weijers
# Use this makefile template for compiling lhs2tex projects
# Replace occurences of word SyntaxDoc with the file you want to compile

commonFiles := $(wildcard ../Common/*)
ltxFiles := $(wildcard *.ltx)
fmtFiles := $(wildcard *.fmt)

all : SyntaxDoc.tmp SyntaxDoc.pdf

SyntaxDoc.tmp : SyntaxDoc.pdf
	open SyntaxDoc.pdf

SyntaxDoc.pdf : ../lit.bib SyntaxDoc.tex  $(commonFiles)
#	svn info | grep 'Revision'          | sed 's/.*\: //' >  ../thesisBin/REVISION
#	svn info | grep 'Last Changed Date' | sed 's/.*(/(/'  >> ../thesisBin/REVISION
	pdflatex -halt-on-error  SyntaxDoc.tex
#	bibtex SyntaxDoc
#	sh -c ' \
#	i=1; \
#	while [ $$i -lt 5 ] && ( \
#	    grep -c "Rerun" SyntaxDoc.log \
#	    || grep -c "undefined citations" SyntaxDoc.log \
#	    || grep -c "undefined references" SyntaxDoc.log); \
#	  do pdflatex -halt-on-error SyntaxDoc.tex; \
#	     i=`expr $$i + 1`; \
#	     done; \
#	   echo "Iterations: $$i"'
	   open SyntaxDoc.pdf	

SyntaxDoc.tex : $(ltxFiles) $(commonFiles) $(fmtFiles)
	rm -f SyntaxDoc.tex
	lhs2TeX --poly SyntaxDoc.ltx >> SyntaxDoc.tex

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
