all: ferryc haddock

configure: ferry.cabal LICENSE Setup.hs
	cabal clean
	cabal configure 

ferryc: configure
	cabal build

clean:
	cabal clean

test:  ferryc
	ghc --make ../test/Main.hs -i../test -i../src -i../bin -outputdir ../bin -o ../bin/tests
	./runShellTests
	./bin/tests

haddock: 
	cabal haddock --internal --executables --hoogle
	cabal haddock --internal --executables
