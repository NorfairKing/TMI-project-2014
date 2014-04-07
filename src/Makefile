all:
	make dirty
	rm -f *.hi *.o

dirty:
	ghc --make -O2 Main

verbose:
	ghc --make -O2 -v Main

fast:
	ghc --make -O3 -fllvm -optlo-O3 -fforce-recomp -v Main
	rm -f *.hi *.o

fastndirty:
	ghc --make -O3 -fllvm -optlo-O3 -fforce-recomp -v Main

pretty:
	hlint *.hs

example:
	make all
	./Main visualize ./test_input/testcase_3_008.txt ./test_output/testcase_3_008.txt > example.svg

test:
	make all
	./Main test all

clean:
	rm -f *.hi *.o *.svg Main
	rm -rf test_input
	rm -rf test_output

benchmark:
	./Main benchmark --samples=1000 --summary=1.csv "algorithm 1"
	./Main benchmark --samples=1000 --summary=2.csv "algorithm 2"
	./Main benchmark --samples=1000 --summary=3.csv "algorithm 3"
