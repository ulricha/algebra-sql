hsFiles :=$(wildcard *.hs) 

all: ferryc haddock

ferryc: language $(hsFiles) bin
	ghc --make Main.hs -i../bin -outputdir ../bin -o ../bin/ferryc

language: ./Ferry/Front/Data/Language.ag bin
	uuagc -cdfswH --self ./Ferry/Front/Data/Language.ag -o ../bin/Ferry/Front/Data/Language.hs

bin:
	sh -c 'mkdir ../bin || true'
	sh -c 'mkdir ../bin/Ferry || true'
	sh -c 'mkdir ../bin/Ferry/Front || true'
	sh -c 'mkdir ../bin/Ferry/Front/Data || true'

doc:
	sh -c 'mkdir ../doc || true'

clean:
	rm -r ../bin
	rm -r ../doc

test: language ferryc
	ghc --make ../test/Main.hs -i../test -i../src -i../bin -outputdir ../bin -o ../bin/tests
	./runShellTests
	../bin/tests

haddock: language doc
	haddock Main.hs -o../doc/ --optghc=-i../bin/ -h --hoogle --source-base=../src/ --source-module=../src/%F -t "Ferry 2.0 Compiler" -w
