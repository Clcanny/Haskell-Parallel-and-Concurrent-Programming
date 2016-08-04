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
	- rm Main.eventlog

compile : clean Main.hs KMeans.hs 
	ghc -O2 Main.hs -threaded -rtsopts -eventlog

points.bin :
	./GenSamples 5 50000 100000 1010

analyse : compile points.bin
	./Main 64 +RTS -l -N4
	threadscope Main.eventlog
