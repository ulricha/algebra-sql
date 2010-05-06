hsFiles :=$(wildcard *.hs) 

all: language $(hsFiles) bin
	ghc --make Main.hs -i../bin -outputdir ../bin -o ../bin/ferryc

language: ./Ferry/Front/Data/Language.ag bin
	uuagc -cdfswH --self ./Ferry/Front/Data/Language.ag -o ../bin/Ferry/Front/Data/Language.hs

bin:
	sh -c 'mkdir ../bin || true'
	sh -c 'mkdir ../bin/Ferry || true'
	sh -c 'mkdir ../bin/Ferry/Front || true'
	sh -c 'mkdir ../bin/Ferry/Front/Data || true'

clean:
	rm -r ../bin

test: language all
	ghc --make ../test/Main.hs -i../test -i../src -i../bin -outputdir ../bin -o ../bin/tests
	./runShellTests
	../bin/tests
