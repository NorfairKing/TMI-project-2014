all:
	make dirty
	rm -f *.hi *.o

dirty:
	ghc --make Main

pretty:
	hlint *.hs

test:
	make all
	./Main test

clean:
	rm -f *.hi *.o Main
