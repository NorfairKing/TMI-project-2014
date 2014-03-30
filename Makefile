.PHONY: verslag

all:
	make dirty
	rm *.hi *.o
	make verslag >/dev/null 2>/dev/null

dirty:
	ghc --make Main

pretty:
	hlint *.hs

verslag:
	$(MAKE) -C verslag

clean:
	rm -f *.hi *.o Main

test:
	make all
