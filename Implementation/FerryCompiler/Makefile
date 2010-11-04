all: ferryc

./dist/setup-config: ferryc.cabal LICENSE Setup.hs
	cabal clean
	cabal configure

configure: ./dist/setup-config

ferryc: configure
	cabal build

clean:
	cabal clean

test:  ferryc
	ghc --make ./test/Main.hs -i./test -i./src -i./dist -outputdir ./dist -o ./dist/tests
	./runShellTests
	./dist/tests

haddock: 
	cabal haddock --internal --executables --hoogle
	cabal haddock --internal --executables

install:
	cabal install
