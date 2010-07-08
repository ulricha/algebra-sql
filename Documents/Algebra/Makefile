# Author: Jeroen Weijers
# Use this makefile template for compiling lhs2tex projects
# Replace occurences of word Template with the file you want to compile

commonFiles := $(wildcard ../Common/*)
ltxFiles := $(wildcard *.ltx)
fmtFiles := $(wildcard *.fmt)

all : Algebra.pdf

Algebra.pdf : ../lit.bib Algebra.tex  $(commonFiles)
#	svn info | grep 'Revision'          | sed 's/.*\: //' >  ../thesisBin/REVISION
#	svn info | grep 'Last Changed Date' | sed 's/.*(/(/'  >> ../thesisBin/REVISION
	pdflatex -halt-on-error  Algebra.tex
	open Algebra.pdf	

Algebra.tex : $(ltxFiles) $(commonFiles) $(fmtFiles)
	rm -f Algebra.tex
	lhs2TeX --poly Algebra.ltx >> Algebra.tex

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
