clean :
	- rm Main.hi
	- rm Main.o
	- rm Main
	- rm KMeansSeq.hi
	- rm KMeansSeq.o
	- rm clusters
	- rm points.bin
	- rm params
	- rm points

compile : clean Main.hs KMeansSeq.hs
	ghc -O2 Main.hs

points.bin :
	./GenSamples 5 50000 100000 1010

run : compile points.bin
	./Main

analyse : compile points.bin
	./Main +RTS -s
