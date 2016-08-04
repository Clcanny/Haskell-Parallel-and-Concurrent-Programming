clean :
	- rm Main.hi
	- rm Main.o
	- rm Main
	- rm KMeans.hi
	- rm KMeans.o
	- rm ParList.hi
	- rm ParList.o
	- rm clusters
	- rm points.bin
	- rm params
	- rm points

compile : clean Main.hs KMeans.hs 
	ghc -O2 Main.hs -threaded -rtsopts

points.bin :
	./GenSamples 5 50000 100000 1010

run : compile points.bin
	./Main

analyse : compile points.bin
	./Main 64 +RTS -s -N1
	./Main 64 +RTS -s -N2
