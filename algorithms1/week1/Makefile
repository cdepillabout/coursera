
default: build
all: default

.PHONY: default all build run test clean configure install-deps

.cabal-sandbox:
	cabal sandbox init

configure: .cabal-sandbox
	cabal configure --enable-tests

install-deps: .cabal-sandbox
	cabal install --only-dependencies --enable-tests -j8

build: install-deps
	cabal build

run: build
	bash -c "time ./dist/build/algorithms1-week1/algorithms1-week1 100 50"

test: build
	cabal test

clean: 
	-rm -rf ./dist
