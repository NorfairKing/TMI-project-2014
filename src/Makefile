all:
	make dirty
	rm -f *.hi *.o

dirty:
	ghc --make -O2 Main

verbose:
	ghc --make -O2 -v Main

pretty:
	hlint *.hs

example:
	make all
	./Main visualize ./test_input/testcase_2_80.txt ./test_output/testcase_2_80.txt > example.svg

test:
	make all
	./Main test all

clean:
	rm -f *.hi *.o Main
	rm -f test_input/*
	rm -f test_output/*
