all:
	make dirty
	rm *.hi *.o

dirty:
	ghc --make compareOutput

pretty:
	hlint *.hs

clean:
	rm -f *.hi *.o compareOutput

test:
	make all
	echo
	./compareOutput ../output/1.txt ../results/1.txt
