all:
	make dirty
	rm -f *.hi *.o

dirty:
	ghc --make -O2 Main

verbose:
	ghc --make -O2 -v Main

pretty:
	hlint *.hs

test:
	make all
	./Main test

clean:
	rm -f *.hi *.o Main
	rm -f test_input/*
	rm -f test_output/*
