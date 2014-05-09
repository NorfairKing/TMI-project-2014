.PHONY: src analysis verslag build


all:
	make clean
	make src
	make analysis
	make verslag
	make build
	make zip
	rm -rf build

final:
	make clean
	$(MAKE) -C src fast
	$(MAKE) -C src test
	$(MAKE) -C src example
	$(MAKE) -C analysis clean-results
	$(MAKE) -C src benchmark
	make analysis
	make verslag
	make build
	make zip
	rm -rf build

src:
	$(MAKE) -C src

verslag:
	$(MAKE) -C verslag

analysis:
	$(MAKE) -C analysis

build:
	mkdir -p build/verslag
	cp verslag/verslag.pdf build/verslag/verslagKerckhoveGoasAguililla.pdf
	mkdir -p build/src
	cp src/Main build/Executable
	cp -r src/* build/src

zip:
	zip -r codeKerckhoveGoasAguililla.zip build
	mkdir -p dist
	mv codeKerckhoveGoasAguililla.zip dist/
 
clean:
	$(MAKE) -C src clean
	$(MAKE) -C analysis clean
	$(MAKE) -C verslag clean
	rm -rf build
	rm -rf dist
