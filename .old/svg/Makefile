
all:
	make dirty
	rm -f *.hi *.o

dirty:
	ghc --make Main

example:
	make all
	./Main ../input/1.txt ../results/1.txt > example.svg

show:
	make example
	display example.svg
clean:
	rm -f *.hi *.o Main
