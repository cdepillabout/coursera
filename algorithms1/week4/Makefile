
default: build
all: default

.PHONY: default all build build-tests run test tests profile profiling clean configure install-deps show-test-log

.cabal-sandbox:
	@echo -e "\nInitting Cabal Sandbox"
	@echo -e "----------------------"
	@cabal sandbox init

dist/profiling:
	@mkdir -p dist/profiling

configure: .cabal-sandbox
	@echo -e "\nRunning Cabal Configure"
	@echo -e "-----------------------"
	@cabal configure --enable-tests --enable-library-coverage --enable-library-profiling --enable-executable-profiling

install-deps: .cabal-sandbox
	@echo -e "\nInstalling project dependencies"
	@echo -e "-------------------------------"
	@cabal install --only-dependencies --enable-tests --enable-library-coverage --enable-library-profiling --enable-executable-profiling -j8

build: install-deps configure
	@echo -e "\nBuilding Project"
	@echo -e "----------------"
	@cabal build

build-tests: install-deps configure
	@echo -e "\nBuilding Tests"
	@echo -e "--------------"
	@cabal build eightpuzzle-tests

run: build
	-@rm -f eightpuzzle.tix
	@echo -e "\nRunning Project"
	@echo -e "---------------"
	@bash -c "time cabal run -- 100 50"

tests test: build-tests
	-@rm -f eightpuzzle.tix
	@echo -e "\nRunning project tests"
	@echo -e "---------------------"
	-@bash -c "time cabal test --test-options='--color --threads 4 --maximum-generated-tests 10000'"
	@echo -e "\nTest Log"
	@echo -e "--------"
	-@bash -c "cat dist/test/*[^0123456789].log"

profile profiling: build dist/profiling
	-@rm -f eightpuzzle.tix
	@echo -e "\nProfiling Project"
	@echo -e "-----------------"
	@cabal run -- +RTS -i0.02 -P -h -s -RTS 100
	@mv eightpuzzle.prof eightpuzzle.hp dist/profiling/
	@echo
	@cat dist/profiling/eightpuzzle.prof
	@bash -c "(cd dist/profiling/ && hp2ps -c -e8in eightpuzzle.hp)"

show-test-log:
	@echo -e "\nLatest Test Log"
	@echo -e "---------------"
	-@bash -c "cat dist/test/*[^0123456789].log"

show-coverage:
	@echo -e "\nLatest Coverage"
	@echo -e "---------------"
	-@bash -c "firefox -no-remote dist/hpc/html/*/hpc_index.html"

show-profiling:
	@echo -e "\nLatest Profiling"
	@echo -e "---------------"
	-@bash -c "evince dist/profiling/eightpuzzle.ps"

clean: 
	@echo -e "\nCleaning"
	@echo -e "--------"
	-rm -rf ./dist
