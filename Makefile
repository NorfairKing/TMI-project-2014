.PHONY: verslag
.PHONY: test

all:
	make dirty
	rm -f *.hi *.o
	make verslag >/dev/null 2>&1
	make test >/dev/null 2>&1

dirty:
	ghc --make Main

pretty:
	hlint *.hs

verslag:
	$(MAKE) -C verslag
	
test:
	$(MAKE) -C test

clean:
	rm -f *.hi *.o Main
