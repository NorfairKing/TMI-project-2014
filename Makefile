.PHONY: verslag

all:
	ghc --make Main
	rm *.hi *.o
	make verslag >/dev/null 2>/dev/null
dirty:
	ghc --make Main

verslag:
	$(MAKE) -C verslag

clean:
	rm -f *.hi *.o Main

test:
	make all
