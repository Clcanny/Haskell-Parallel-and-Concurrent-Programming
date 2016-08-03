clean :
	- rm Main.hi
	- rm Main.o
	- rm Main
	- rm KMeansList.hi
	- rm KMeansList.o
	- rm KMeansVector.hi
	- rm KMeansVector.o
	- rm KMeansVector2.hi
	- rm KMeansVector2.o
	- rm KMeansVector3.hi
	- rm KMeansVector3.o
	- rm KMeansVector4.hi
	- rm KMeansVector4.o
	- rm KMeans5.hi
	- rm KMeans5.o
	- rm clusters
	- rm points.bin
	- rm params
	- rm points

compile : clean Main.hs KMeansList.hs KMeansVector.hs
	ghc -O2 Main.hs

points.bin :
	./GenSamples 5 50000 100000 1010

run : compile points.bin
	./Main

analyse : compile points.bin
	./Main +RTS -s
