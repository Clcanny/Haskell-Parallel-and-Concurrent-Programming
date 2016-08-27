compile : parallelizing.hs
	ghc -O2 -threaded -fllvm -fforce-recomp -rtsopts -eventlog parallelizing.hs

run : compile
	./parallelizing seq +RTS -s -N4

clean :
	- rm *.o
	- rm *.hi
	- rm parallelizing
