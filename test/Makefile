all:
	make dirty
	rm *.hi *.o
	# make generate
	make test

dirty:
	ghc --make Main

pretty:
	hlint *.hs

clean:
	rm -f *.hi *.o compareOutput generateRandomCases

generate:
	./generateRandomCases >/dev/null 2>&1

test:
	echo
	./compareOutput ../output/1.txt ../results/1.txt
	echo
