.PHONY: Intersections, Geometry, Test, Visual, Benchmark

all:
	make dirty

dirty:
	ghc --make -O2 Main

verbose:
	ghc --make -O2 Main

fast:
	ghc --make -O3 -fforce-recomp Main
	rm -f *.hi *.o

pretty:
	hlint *.hs
	ghc --make -fwarn-unused-imports Main

example:
	make all
	./Main visualize ./test_input/testcase_3_020.txt ./test_output/testcase_3_020.txt > example.svg

test:
	make all
	./Main test all

fasttest:
	make fast
	./Main test all

clean:
	rm -f *.hi *.o *.svg Main
	rm -rf test_input
	rm -rf test_output
	rm -rf benchmarks
	$(MAKE) -C Test clean
	$(MAKE) -C Visual clean
	$(MAKE) -C Geometry clean
	$(MAKE) -C Intersections clean
	$(MAKE) -C Benchmark clean

benchmark:
	./Main benchmark
