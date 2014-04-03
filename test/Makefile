all:
	make dirty
	rm *.hi *.o
	make test

dirty:
	ghc --make Main

pretty:
	hlint *.hs

clean:
	rm -f *.hi *.o Main
	rm -f testInput/testcase*
	rm -f testResults/testcase*

generate:
	./Main generate

run:
	./Main run

compare:
	./Main compare

test:
	make generate
	make run
	make compare
