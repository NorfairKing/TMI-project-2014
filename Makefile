all:
	ghc --make Main
	rm *.hi *.o

dirty:
	ghc --make Main

clean:
	rm -f *.hi *.o Main

test:
	make all
