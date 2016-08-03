GenSamples : GenSamples.hs
	ghc -O2 GenSamples.hs

points.bin : GenSamples
	./GenSamples 5 50000 100000 1010

compile : clean
	ghc -O2 -threaded -rtsopts -eventlog kmeans.hs

runSeq : compile points.bin
	./kmeans seq

analyseSeq : compile points.bin
	./kmeans seq +RTS -s

clean :
	- rm GenSamples.hi
	- rm GenSamples.o
	- rm GenSamples
	- rm KMeansCore.hi
	- rm KMeansCore.o
	- rm kmeans.hi
	- rm kmeans.o
	- rm kmeans
	- rm Random.hi
	- rm Random.o
	- rm points
	- rm points.bin
	- rm clusters
	- rm params
