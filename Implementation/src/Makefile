hsFiles :=$(wildcard *.hs) 

all: ferryc haddock

configure:
    cabal configure 
ferryc: $(hsFiles) bin
	ghc --make Main.hs -i../bin -outputdir ../bin -o ../bin/ferryc

# core: ./Ferry/Core/Data/Core.ag bin
# 	uuagc -cdfswH --self ./Ferry/Core/Data/Core.ag -o ../bin/Ferry/Core/Data/Core.hs   

#language: ./Ferry/Front/Data/Language.ag ./Ferry/Front/Data/AST.ag bin
#	uuagc -cdfswH --self ./Ferry/Front/Data/Language.ag -o ../bin/Ferry/Front/Data/Language.hs

#pretty: ./Ferry/Front/Render/PrettyFerry.ag ./Ferry/Front/Data/AST.ag bin
#	uuagc -cdfswH --self ./Ferry/Front/Render/PrettyFerry.ag -o ../bin/Ferry/Front/Render/PrettyFerry.hs

bin:
	sh -c 'mkdir ../bin || true'
#	sh -c 'mkdir ../bin/Ferry || true'
#	sh -c 'mkdir ../bin/Ferry/Front || true'
#	sh -c 'mkdir ../bin/Ferry/Front/Data || true'
#	sh -c 'mkdir ../bin/Ferry/Core || true'
#	sh -c 'mkdir ../bin/Ferry/Core/Data || true'
#	sh -c 'mkdir ../bin/Ferry/Front/Render || true'

doc:
	sh -c 'mkdir ../doc || true'

clean:
	rm -r ../bin
	rm -r ../doc

test:  ferryc
	ghc --make ../test/Main.hs -i../test -i../src -i../bin -outputdir ../bin -o ../bin/tests
	./runShellTests
	../bin/tests

haddock: doc
	haddock Main.hs -o../doc/ --optghc=-i../bin/ -h --hoogle --source-base=../src/ --source-module=../src/%F -t "Ferry 2.0 Compiler" -w
