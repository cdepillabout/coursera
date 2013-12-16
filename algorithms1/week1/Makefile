
default: build
all: default

.PHONY: default all build build-tests run test clean configure install-deps show-test-log

.cabal-sandbox:
	@echo -e "\nInitting Cabal Sandbox"
	@echo -e "----------------------"
	@cabal sandbox init

dist/test/algorithms1-week1-0.1.0.0-alltests.log: test

configure: .cabal-sandbox
	@echo -e "\nRunning Cabal Configure"
	@echo -e "-----------------------"
	@cabal configure --enable-tests --enable-library-coverage

install-deps: .cabal-sandbox
	@echo -e "\nInstalling project dependencies"
	@echo -e "-------------------------------"
	@cabal install --only-dependencies --enable-tests -j8

build: install-deps
	@echo -e "\nBuilding Project"
	@echo -e "----------------"
	@cabal build

build-tests: install-deps
	@echo -e "\nBuilding Tests"
	@echo -e "--------------"
	@cabal build alltests

run: build
	@echo -e "\nRunning Project"
	@echo -e "---------------"
	@bash -c "time ./dist/build/algorithms1-week1/algorithms1-week1 100 50"

test: build-tests
	@echo -e "\nRunning project tests"
	@echo -e "---------------------"
	#@bash -c "time ./dist/build/alltests/alltests --num-threads 4 +RTS -N4 -RTS --quickcheck-tests 10000"
	@bash -c "time ./dist/build/alltests/alltests --threads 4 --maximum-generated-tests 10000 --hide-successes"


show-test-log: dist/test/algorithms1-week1-0.1.0.0-alltests.log
	@echo -e "\nLatest Test Log"
	@echo -e "---------------"
	@cat "dist/test/algorithms1-week1-0.1.0.0-alltests.log"

clean: 
	@echo -e "\nCleaning"
	@echo -e "\n------"
	-rm -rf ./dist
