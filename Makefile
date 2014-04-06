.PHONY: code verslag build


all:
	make src
	make verslag
	make build
	make zip
	rm -rf build

src:
	$(MAKE) -C src

verslag:
	$(MAKE) -C verslag

build:
	mkdir -p build/verslag
	cp verslag/verslag.pdf build/verslag/verslagKerckhoveGoasAguililla.pdf
	mkdir -p build/code
	cp src/Main build/Executable
	cp src/*.hs build/code

zip:
	zip -r codeKerckhoveGoasAguililla.zip build
	mkdir dist
	mv codeKerckhoveGoasAguililla.zip dist/

clean:
	rm -rf build
	rm -rf dist
