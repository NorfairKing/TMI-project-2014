all:
	make dirty
	rm *.hi *.o

dirty:
	ghc --make compareOutput
	ghc --make generateRandomCase

pretty:
	hlint *.hs

clean:
	rm -f *.hi *.o compareOutput generateRandomCase

test:
	make all
	echo
	./compareOutput ../output/1.txt ../results/1.txt
