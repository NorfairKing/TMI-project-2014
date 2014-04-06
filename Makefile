.PHONY: code verslag build


all:
	make code
	make verslag
	make build
	make zip
	rm -rf build

code:
	$(MAKE) -C code

verslag:
	$(MAKE) -C verslag

build:
	mkdir -p build/verslag
	cp verslag/verslag.pdf build/verslag/verslagKerckhoveGoasAguililla.pdf
	mkdir -p build/code
	cp code/Main build/Executable
	cp code/*.hs build/code

zip:
	zip -r codeKerckhoveGoasAguililla.zip build

clean:
	rm -rf build
	rm codeKerckhoveGoasAguililla.zip
