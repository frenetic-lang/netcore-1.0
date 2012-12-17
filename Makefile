all:
	cabal configure --enable-tests
	cabal build
	cabal install
