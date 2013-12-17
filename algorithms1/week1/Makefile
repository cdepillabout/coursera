
default: build
all: default

.PHONY: default all build build-tests run test clean configure install-deps show-test-log

.cabal-sandbox:
	@echo -e "\nInitting Cabal Sandbox"
	@echo -e "----------------------"
	@cabal sandbox init

configure: .cabal-sandbox
	@echo -e "\nRunning Cabal Configure"
	@echo -e "-----------------------"
	@cabal configure --enable-tests --enable-library-coverage

install-deps: .cabal-sandbox
	@echo -e "\nInstalling project dependencies"
	@echo -e "-------------------------------"
	@cabal install --only-dependencies --enable-tests -j8

build: install-deps configure
	@echo -e "\nBuilding Project"
	@echo -e "----------------"
	@cabal build

build-tests: install-deps configure
	@echo -e "\nBuilding Tests"
	@echo -e "--------------"
	@cabal build alltests

run: build
	@echo -e "\nRunning Project"
	@echo -e "---------------"
	@bash -c "time cabal run -- 100 50"

test: build-tests
	@echo -e "\nRunning project tests"
	@echo -e "---------------------"
	-@bash -c "time cabal test --test-options='--color --threads 4 --maximum-generated-tests 10000'"
	@echo -e "\nTest Log"
	@echo -e "--------"
	-@bash -c "cat dist/test/*[^0123456789].log"

show-test-log:
	@echo -e "\nLatest Test Log"
	@echo -e "---------------"
	-@bash -c "cat dist/test/*[^0123456789].log"

clean: 
	@echo -e "\nCleaning"
	@echo -e "--------"
	-rm -rf ./dist
