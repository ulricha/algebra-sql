all: ferryCore

./dist/setup-config: ferryCore.cabal LICENSE Setup.hs
	cabal clean
	cabal configure

configure: ./dist/setup-config

ferryCore: configure
	cabal build

clean:
	cabal clean

haddock: 
	cabal haddock --internal --executables --hoogle
	cabal haddock --internal --executables

install:
	cabal install
